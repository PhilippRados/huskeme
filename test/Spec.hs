{-# LANGUAGE OverloadedStrings #-}

import Control.Exception.Base
import Data.Either
import Data.Text (Text)
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
      assertEval "(+ 2 3)" (Number 5)
      assertEval "(+ 2 3 9)" (Number 14)
      assertEval "(- 2 3 1)" (Number (-2))

    it "nested math" $ do
      assertEval "(+ (* 2 2) 3)" (Number 7)
      assertEval "(* (+ 2 (* 4 6)) (+ 3 5 7))" (Number 390)

    it "num comp" $ do
      assertEval "(= 1 1)" (Bool True)
      assertEval "(= 1 1 1)" (Bool True)
      assertEval "(= 1 1 2)" (Bool False)
      assertEval "(<= 1 1 2)" (Bool True)
      assertEval "(<= 1 0 2)" (Bool False)
      assertEval "(< 1 2 3)" (Bool True)
      assertEval "(< 1 5 4)" (Bool False)

    it "str comp" $ do
      assertEval "(string=? \"foo\" \"foo\")" (Bool True)
      assertEval "(string=? \"foo\" \"bar\")" (Bool False)
      assertEval "(string=? \"foo\" \"foo\" \"bar\")" (Bool False)
      assertEval "(string<? \"abc\" \"bba\")" (Bool True)
      assertEval "(string<? \"abc\" \"aba\")" (Bool False)
      assertEval "(string<=? \"abc\" \"abc\")" (Bool True)

    it "if cond" $ do
      assertEval "(if #t 1 2)" (Number 1)
      assertEval "(if #f 1 2)" (Number 2)
      assertEval "(if \"foo\" 1 2)" (Number 2)
      assertEval "(if #t 1)" (Number 1)
      assertEval "(if #f 1)" Undefined
      assertEval "((if #f - *) 3 4)" (Number 12)

assertEval :: (HasCallStack) => Text -> LispVal -> Expectation
assertEval expr expected =
  evalExpr <$> readExpr expr `shouldBe` Right expected
