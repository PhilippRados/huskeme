{-# LANGUAGE OverloadedStrings #-}

import Control.Exception.Base
import Data.Either
import Data.Text (Text)
import Error
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

    it "parses quote" $ do
      readExpr "'(1 2 3)" `shouldBe` Right (List [Atom "quote", List [Number 1, Number 2, Number 3]])
      readExpr "'(+ 1 2)" `shouldBe` Right (List [Atom "quote", List [Atom "+", Number 1, Number 2]])
      readExpr "(quote (+ 1 2))" `shouldBe` Right (List [Atom "quote", List [Atom "+", Number 1, Number 2]])

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
      assertEvalErr "(= \"1\" \"1\")"
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
      assertEvalErr "(string=? 1 2 3)"

    it "if cond" $ do
      assertEval "(if #t 1 2)" (Number 1)
      assertEval "(if #f 1 2)" (Number 2)
      assertEval "(if \"foo\" 1 2)" (Number 2)
      assertEval "(if #t 1)" (Number 1)
      assertEval "(if #f 1)" Undefined
      assertEval "((if #f - *) 3 4)" (Number 12)

    it "car operator" $ do
      assertEval "(car '(a b c))" (Atom "a")
      assertEval "(car '((a) b c d))" (List [Atom "a"])
      assertEval "(car '(1 . 2))" (Number 1)
      assertEvalErr "(car '())"

    it "cdr operator" $ do
      assertEval "(cdr '((a) b c d))" (List [Atom "b", Atom "c", Atom "d"])
      assertEval "(cdr '(1 . 2))" (Number 2)
      assertEval "(cdr '(1))" (List [])
      assertEvalErr "(cdr '())"

    it "cons operator" $ do
      assertEval "(cons 'a '())" (List [Atom "a"])
      assertEval "(cons '() '())" (List [List []])
      assertEval "(cons '(a) '(b c d))" (List [List [Atom "a"], Atom "b", Atom "c", Atom "d"])
      assertEval "(cons \"a\" '(b c))" (List [String "a", Atom "b", Atom "c"])
      assertEval "(cons 'a 3)" (DottedList [Atom "a"] (Number 3))
      assertEval "(cons '(a b) 'c)" (DottedList [List [Atom "a", Atom "b"]] (Atom "c"))

    it "equality operations" $ do
      assertEval "(eqv? 'a 'a)" (Bool True)
      assertEval "(eqv? 'a 'b)" (Bool False)
      assertEval "(eqv? 2 2)" (Bool True)
      assertEval "(eqv? '() '())" (Bool True)
      assertEval "(eqv? 100000000 100000000)" (Bool True)
      assertEval "(eqv? (cons 1 2) (cons 1 2))" (Bool False)
      assertEval "(eqv? '('a 'b) '('a 'b))" (Bool False)
      assertEval "(eqv? \"abc\" \"abc\")" (Bool True)

      assertEval "(equal? '('a 'b) '('a 'b))" (Bool True)
      assertEval "(equal? (cons 1 2) (cons 1 2))" (Bool True)
      assertEval "(equal? 'a 'a)" (Bool True)
      assertEval "(equal? '(a) '(a))" (Bool True)
      assertEval "(equal? '(a (b) c) '(a (b) c))" (Bool True)
      assertEval "(equal? \"abc\" \"abc\")" (Bool True)
      assertEval "(equal? 2 2)" (Bool True)
      assertEval "(equal? 1 \"1\")" (Bool False)

assertEval :: (HasCallStack) => Text -> LispVal -> Expectation
assertEval expr expected =
  (readExpr expr >>= eval) `shouldBe` Right expected

assertEvalErr :: (HasCallStack) => Text -> Expectation
assertEvalErr expr =
  (readExpr expr >>= eval) `shouldSatisfyWithMessage` isLeft

shouldSatisfyWithMessage :: (Show a) => a -> (a -> Bool) -> Expectation
shouldSatisfyWithMessage actual predicate =
  if predicate actual
    then return () -- Test passes
    else expectationFailure $ "Predicate failed on: " ++ show actual -- Print the actual value
