{-# LANGUAGE OverloadedStrings #-}

import Control.Exception.Base
import Control.Monad.IO.Class (liftIO)
import Data.Either
import Data.Text (Text, pack)
import Error
import Eval
import Parser
import Repl
import Test.Hspec
import Types

main :: IO ()
main = hspec $ do
  testParse
  testEval
  testFixtures

testParse =
  describe "readExpr" $ do
    it "parses bools" $ do
      assertParse "#t" [Bool True]
      assertParse "#f" [Bool False]

    it "parses strings" $ do
      assertParse "\"hello\"" [String "hello"]
      assertParse "\"\"" [String ""]

    it "parses atoms" $ do
      assertParse "abc" [Atom "abc"]
      assertParse "a1bc" [Atom "a1bc"]
      assertParseErr "(1abc)"

    it "parses nums" $ do
      assertParse "1 abc" [Number 1, Atom "abc"]

    it "parses lists" $ do
      assertParse "(+ 2 3)" [List [Atom "+", Number 2, Number 3]]
      assertParse "(+ (- 2 10) 3)" [List [Atom "+", List [Atom "-", Number 2, Number 10], Number 3]]

    it "parses dotted-lists" $ do
      assertParse "(1 .  (2 . 3))" [DottedList [Number 1, Number 2] (Number 3)]

    it "parses quote" $ do
      assertParse "'(1 2 3)" [List [Atom "quote", List [Number 1, Number 2, Number 3]]]
      assertParse "'(+ 1 2)" [List [Atom "quote", List [Atom "+", Number 1, Number 2]]]
      assertParse "(quote (+ 1 2))" [List [Atom "quote", List [Atom "+", Number 1, Number 2]]]

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
      assertEval "(if \"foo\" 1 2)" (Number 1)
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
      assertEval "(cons 1 '(2 . ()))" (List [Number 1, Number 2])

    it "dot operations" $ do
      assertEval "(+ . (5 6))" (Number 11)
      assertEval "'(+ . (5 6))" (List [Atom "+", Number 5, Number 6])
      assertEval "'(+ . (5 . (6 . ())))" (List [Atom "+", Number 5, Number 6])
      assertEval "'(+ . (5 . 6))" (DottedList [Atom "+", Number 5] (Number 6))
      assertEval "'(+ 1 2 . (5 . (6 7 . ())))" (List [Atom "+", Number 1, Number 2, Number 5, Number 6, Number 7])
      assertEval "(+ 1 . (5 6))" (Number 12)
      assertEvalErr "(+ . (5 . 6))"
      assertEvalErr "(cons + (5 6))"
      assertEvalErr "((+ 1 1) . (3))"
      assertEvalErr "(cons + (3))"

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

    it "logical operations" $ do
      assertEval "(and 1 2)" (Number 2)
      assertEval "(and #f 2)" (Bool False)
      assertEval "(and #t 6)" (Number 6)
      assertEval "(and (= 2 2) (> 2 1))" (Bool True)
      assertEval "(and (= 2 2) (< 2 1))" (Bool False)
      assertEval "(and 1 2 'c '(f g))" (List [Atom "f", Atom "g"])
      assertEval "(and)" (Bool True)

      assertEval "(or 1 2)" (Number 1)
      assertEval "(or #f #f 0 #f)" (Number 0)
      assertEval "(or 1 #t)" (Number 1)
      assertEval "(or (= 2 2) (> 2 1))" (Bool True)
      assertEval "(or (= 2 2) (< 2 1))" (Bool True)
      assertEval "(or #f #f #f)" (Bool False)

testFixtures =
  describe "fixtures" $ do
    it "nested variables" $ do
      assertFile "arith.scm" (Number (-5))

    it "global variables and functions" $ do
      assertFile "functions.scm" (Number 21)

    it "recursive functions" $ do
      assertFile "factorial.scm" (Number 3628800)

    it "currying functions" $ do
      assertFile "curry.scm" (Number 9)

assertFile :: (HasCallStack) => String -> LispVal -> Expectation
assertFile file expected = do
  contents <- liftIO $ readFile ("fixtures/" ++ file)
  (readExprs (pack contents) >>= eval) `shouldBe` Right expected

assertParse :: (HasCallStack) => Text -> [LispVal] -> Expectation
assertParse exprs expected =
  readExprs exprs `shouldBe` Right expected

assertParseErr :: (HasCallStack) => Text -> Expectation
assertParseErr expr =
  readExprs expr `shouldSatisfyWithMessage` isLeft

assertEval :: (HasCallStack) => Text -> LispVal -> Expectation
assertEval expr expected =
  (readExprs expr >>= eval) `shouldBe` Right expected

assertEvalErr :: (HasCallStack) => Text -> Expectation
assertEvalErr expr =
  (readExprs expr >>= eval) `shouldSatisfyWithMessage` isLeft

shouldSatisfyWithMessage :: (Show a) => a -> (a -> Bool) -> Expectation
shouldSatisfyWithMessage actual predicate =
  if predicate actual
    then return () -- Test passes
    else expectationFailure $ "Predicate failed on: " ++ show actual -- Print the actual value
