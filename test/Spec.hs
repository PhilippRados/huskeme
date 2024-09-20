{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Parser
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "readExpr" $ do
    it "parses bools" $ do
      readExpr "#t" `shouldBe` Right (Bool True)
      readExpr "#f" `shouldBe` Right (Bool False)

    it "parses strings" $ do
      readExpr "\"hello\"" `shouldBe` Right (String "hello")
      readExpr "\"\"" `shouldBe` Right (String "")

    it "parses nil" $ do
      readExpr "nil" `shouldBe` Right Nil

    it "parses atoms" $ do
      readExpr "abc" `shouldBe` Right (Atom "abc")
      readExpr "a1bc" `shouldBe` Right (Atom "a1bc")
      readExpr "1abc" `shouldSatisfy` isLeft

    it "parses nums" $ do
      readExpr "1 abc" `shouldBe` Right (Number 1)
