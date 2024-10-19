{-# LANGUAGE FlexibleContexts #-}

module Repl (runRepl) where

import Builtins (builtinEnv)
import Control.Monad.Except
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import Eval
import System.Console.Repline
import Utils (Env (..), printError)

type Repl a = HaskelineT (StateT Env IO) a

cmd :: String -> Repl ()
cmd input = do
  env <- get
  result <- liftIO $ runExceptT $ runStateT (runWithEnv input "<stdin>") env
  case result of
    Left err -> liftIO $ printError err
    Right (val, env') -> do
      lift $ modify $ const env'
      liftIO $ print val

flattenKeys :: Env -> [String]
flattenKeys env = nub $ map T.unpack $ Map.keys (envRefs env)

comp :: (Monad m, MonadState Env m) => WordCompleter m
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
ini =
  liftIO $ putStrLn $ unlines ["huskeme 0.1.0.0", "MIT License", "Philipp Rados", "Type :help for help, <Ctrl-D> to quit"]

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

runRepl :: IO ()
runRepl =
  flip evalStateT builtinEnv $
    evalRepl (const $ pure "Repl> ") cmd opts (Just ':') Nothing (Word comp) ini final
