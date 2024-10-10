{-# LANGUAGE FlexibleContexts #-}

module Repl (runRepl) where

import Builtins (builtinEnv)
import Control.Monad.Except
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Version (showVersion)
import Distribution.PackageDescription (GenericPackageDescription, PackageDescription (..), PackageIdentifier (..), package, packageDescription)
import Distribution.Simple.PackageDescription
import Distribution.Types.PackageDescription
import Distribution.Types.PackageName (unPackageName)
import Distribution.Verbosity (normal)
import Error
import Eval
import System.Console.Repline
import Text.Show.Pretty
import Types (Env)

type Repl a = HaskelineT (StateT [Env] IO) a

cmd :: String -> Repl ()
cmd input = do
  env <- get
  result <- liftIO $ runExceptT $ runStateT (runWithEnv input) env
  case result of
    Left err -> liftIO $ putStrLn $ formatError err input
    Right (val, env') -> do
      lift $ modify $ const env'
      liftIO $ print val

flattenKeys :: [Env] -> [String]
flattenKeys env = nub $ map T.unpack $ concatMap Map.keys env

comp :: (Monad m, MonadState [Env] m) => WordCompleter m
comp n = do
  env <- get
  let symbols = flattenKeys env
  return $ filter (isPrefixOf n) symbols

help :: [String] -> Repl ()
help _ = liftIO $ putStrLn $ info ++ formatCommands
  where
    info = "Commands available from the prompt:\n"
    formatCommands = unlines $ map (\(c, desc) -> "\t:" ++ c ++ "\t\t" ++ desc) commands
    commands =
      [ ("help", "show this help overview"),
        ("env", "dump all the symbols from the environment")
      ]

dumpEnv :: [String] -> Repl ()
dumpEnv _ = do
  env <- get
  liftIO $ putStrLn $ unlines $ flattenKeys env

opts :: [(String, String -> Repl ())]
opts =
  [ ("help", help . words),
    ("env", dumpEnv . words)
  ]

ini :: Repl ()
ini = do
  (name, version, plicense, pauthor) <- liftIO $ getPackageInfo "lispeln.cabal"
  liftIO $ putStrLn $ unlines [name ++ version, plicense, pauthor, "Type :help for help, <Ctrl-D> to quit"]

getPackageInfo :: FilePath -> IO (String, String, String, String)
getPackageInfo cabalFile = do
  gpd <- readGenericPackageDescription normal cabalFile
  let pd = packageDescription gpd
      PackageIdentifier {pkgName = pkgName, pkgVersion = pkgVersion} = package pd
      name = unPackageName pkgName
      version = prettyShow pkgVersion
      pAuthor = show $ author pd
      plicense = license pd
  return (name, version, plicense, pAuthor)

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

runRepl :: IO ()
runRepl =
  flip evalStateT builtinEnv $
    evalRepl (const $ pure "Repl> ") cmd opts (Just ':') Nothing (Word comp) ini final
