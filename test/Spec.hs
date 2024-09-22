{-# LANGUAGE OverloadedStrings #-}

import Data.Either
import Eval
import Parser
import Test.Hspec

main :: IO ()
main = hspec $ do
  testParse
  testEval

testParse =
  describe "readExpr" $ do
    it "parses bools" $ do
      readExpr "#t" `shouldBe` Right (Bool True)
      readExpr "#f" `shouldBe` Right (Bool False)

    it "parses strings" $ do
      readExpr "\"hello\"" `shouldBe` Right (String "hello")
      readExpr "\"\"" `shouldBe` Right (String "")

    it "parses atoms" $ do
      readExpr "abc" `shouldBe` Right (Atom "abc")
      readExpr "a1bc" `shouldBe` Right (Atom "a1bc")
      readExpr "(1abc)" `shouldSatisfy` isLeft

    it "parses nums" $ do
      readExpr "1 abc" `shouldBe` Right (Number 1)

    it "parses lists" $ do
      readExpr "(+ 2 3)" `shouldBe` Right (List [Atom "+", Number 2, Number 3])
      readExpr "(+ (- 2 10) 3)" `shouldBe` Right (List [Atom "+", List [Atom "-", Number 2, Number 10], Number 3])

    it "parses dotted-lists" $ do
      readExpr "(1 .  (2 . 3))" `shouldBe` Right (DottedList [Number 1] (DottedList [Number 2] (Number 3)))

testEval =
  describe "evalExpr" $ do
    it "simple math" $ do
      evalExpr <$> readExpr "(+ 2 3)" `shouldBe` Right (Number 5)
      evalExpr <$> readExpr "(+ 2 3 9)" `shouldBe` Right (Number 14)
      evalExpr <$> readExpr "(- 2 3 1)" `shouldBe` Right (Number (-2))

    it "nested math" $ do
      evalExpr <$> readExpr "(+ (* 2 2) 3)" `shouldBe` Right (Number 7)
      evalExpr <$> readExpr "(* (+ 2 (* 4 6)) (+ 3 5 7))" `shouldBe` Right (Number 390)

    it "num comp" $ do
      evalExpr <$> readExpr "(= 1 1)" `shouldBe` Right (Bool True)
      evalExpr <$> readExpr "(= 1 1 1)" `shouldBe` Right (Bool True)
      evalExpr <$> readExpr "(= 1 1 2)" `shouldBe` Right (Bool False)

    it "str comp" $ do
      evalExpr <$> readExpr "(string=? \"foo\" \"foo\")" `shouldBe` Right (Bool True)
      evalExpr <$> readExpr "(string=? \"foo\" \"foo\" \"bar\")" `shouldBe` Right (Bool False)
      evalExpr <$> readExpr "(string=? \"foo\" \"bar\")" `shouldBe` Right (Bool False)
      evalExpr <$> readExpr "(string<? \"abc\" \"bba\")" `shouldBe` Right (Bool True)
