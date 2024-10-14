{-# LANGUAGE OverloadedStrings #-}

import Control.Exception.Base
import Control.Monad.IO.Class (liftIO)
import Data.Either
import Data.Text (Text, pack)
import Eval
import Parser
import Repl
import Test.Hspec
import Text.Parsec.Pos (SourcePos, newPos)
import Utils

mockLoc :: Loc
mockLoc = Loc {locPos = newPos "testfile" 1 1, locInput = ""}

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
      assertParse "abc" [Atom "abc" mockLoc]
      assertParse "a1bc" [Atom "a1bc" mockLoc]
      assertParse "-1foo" [Atom "-1foo" mockLoc]
      assertParseErr "(1abc)"

    it "parses nums" $ do
      assertParse "1 abc" [Number 1, Atom "abc" mockLoc]
      assertParse "+1" [Number 1]
      assertParse "-1" [Number (-1)]
      assertParse "(- 1 1)" [List [Atom "-" mockLoc, Number 1, Number 1] mockLoc]

    it "parses lists" $ do
      assertParse "(+ 2 3)" [List [Atom "+" mockLoc, Number 2, Number 3] mockLoc]
      assertParse "(+ (- 2 10) 3)" [List [Atom "+" mockLoc, List [Atom "-" mockLoc, Number 2, Number 10] mockLoc, Number 3] mockLoc]

    it "parses dotted-lists" $ do
      assertParse "(1 .  (2 . 3))" [DottedList [Number 1, Number 2] (Number 3) mockLoc]
      assertParse "(define (foo . args) (car args))" [List [Atom "define" mockLoc, DottedList [Atom "foo" mockLoc] (Atom "args" mockLoc) mockLoc, List [Atom "car" mockLoc, Atom "args" mockLoc] mockLoc] mockLoc]

    it "parses quote" $ do
      assertParse "'(1 2 3)" [List [Atom "quote" mockLoc, List [Number 1, Number 2, Number 3] mockLoc] mockLoc]
      assertParse "'(+ 1 2)" [List [Atom "quote" mockLoc, List [Atom "+" mockLoc, Number 1, Number 2] mockLoc] mockLoc]
      assertParse "(quote (+ 1 2))" [List [Atom "quote" mockLoc, List [Atom "+" mockLoc, Number 1, Number 2] mockLoc] mockLoc]

testEval =
  describe "evalExpr" $ do
    it "simple math" $ do
      assertEval "(+ 2 3)" "5"
      assertEval "(+ 2 3 9)" "14"
      assertEval "(- 2 3 1)" "-2"
      assertEval "(+ -3 -8 4)" "-7"

    it "nested math" $ do
      assertEval "(+ (* 2 2) 3)" "7"
      assertEval "(* (+ 2 (* 4 6)) (+ 3 5 7))" "390"
      assertEval "(+ (- 3 -8) 4)" "15"

    it "num comp" $ do
      assertEval "(= 1 1)" "#t"
      assertEval "(= 1 1 1)" "#t"
      assertEval "(= 1 1 2)" "#f"
      assertEvalErr "(= \"1\" \"1\")"
      assertEval "(<= 1 1 2)" "#t"
      assertEval "(<= 1 0 2)" "#f"
      assertEval "(< 1 2 3)" "#t"
      assertEval "(< 1 5 4)" "#f"

    it "str comp" $ do
      assertEval "(string=? \"foo\" \"foo\")" "#t"
      assertEval "(string=? \"foo\" \"bar\")" "#f"
      assertEval "(string=? \"foo\" \"foo\" \"bar\")" "#f"
      assertEval "(string<? \"abc\" \"bba\")" "#t"
      assertEval "(string<? \"abc\" \"aba\")" "#f"
      assertEval "(string<=? \"abc\" \"abc\")" "#t"
      assertEvalErr "(string=? 1 2 3)"

    it "if cond" $ do
      assertEval "(if #t 1 2)" "1"
      assertEval "(if #f 1 2)" "2"
      assertEval "(if \"foo\" 1 2)" "1"
      assertEval "(if #t 1)" "1"
      assertEval "(if #f 1)" "<undefined>"
      assertEval "((if #f - *) 3 4)" "12"

    it "car operator" $ do
      assertEval "(car '(a b c))" "a"
      assertEval "(car '((a) b c d))" "(a)"
      assertEval "(car '(1 . 2))" "1"
      assertEvalErr "(car '())"

    it "cdr operator" $ do
      assertEval "(cdr '((a) b c d))" "(b c d)"
      assertEval "(cdr '(1 . 2))" "2"
      assertEval "(cdr '(1))" "()"
      assertEvalErr "(cdr '())"

    it "cons operator" $ do
      assertEval "(cons 'a '())" "(a)"
      assertEval "(cons '() '())" "(())"
      assertEval "(cons '(a) '(b c d))" "((a) b c d)"
      assertEval "(cons \"a\" '(b c))" "(\"a\" b c)"
      assertEval "(cons 'a 3)" "(a . 3)"
      assertEval "(cons '(a b) 'c)" "((a b) . c)"
      assertEval "(cons 1 '(2 . ()))" "(1 2)"

    it "dot operations" $ do
      assertEval "(+ . (5 6))" "11"
      assertEval "'(+ . (5 6))" "(+ 5 6)"
      assertEval "'(+ . (5 . (6 . ())))" "(+ 5 6)"
      assertEval "'(+ . (5 . 6))" "(+ 5 . 6)"
      assertEval "'(+ 1 2 . (5 . (6 7 . ())))" "(+ 1 2 5 6 7)"
      assertEval "(+ 1 . (5 6))" "12"
      assertEvalErr "(+ . (5 . 6))"
      assertEvalErr "(cons + (5 6))"
      assertEvalErr "((+ 1 1) . (3))"
      assertEvalErr "(cons + (3))"

    it "equality operations" $ do
      assertEval "(eqv? 'a 'a)" "#t"
      assertEval "(eqv? 'a 'b)" "#f"
      assertEval "(eqv? 2 2)" "#t"
      assertEval "(eqv? '() '())" "#t"
      assertEval "(eqv? 100000000 100000000)" "#t"
      assertEval "(eqv? (cons 1 2) (cons 1 2))" "#f"
      assertEval "(eqv? '('a 'b) '('a 'b))" "#f"
      assertEval "(eqv? \"abc\" \"abc\")" "#t"

      assertEval "(equal? '('a 'b) '('a 'b))" "#t"
      assertEval "(equal? (cons 1 2) (cons 1 2))" "#t"
      assertEval "(equal? 'a 'a)" "#t"
      assertEval "(equal? '(a) '(a))" "#t"
      assertEval "(equal? '(a (b) c) '(a (b) c))" "#t"
      assertEval "(equal? \"abc\" \"abc\")" "#t"
      assertEval "(equal? 2 2)" "#t"
      assertEval "(equal? 1 \"1\")" "#f"

    it "logical operations" $ do
      assertEval "(and 1 2)" "2"
      assertEval "(and #f 2)" "#f"
      assertEval "(and #t 6)" "6"
      assertEval "(and (= 2 2) (> 2 1))" "#t"
      assertEval "(and (= 2 2) (< 2 1))" "#f"
      assertEval "(and 1 2 'c '(f g))" "(f g)"
      assertEval "(and)" "#t"

      assertEval "(or 1 2)" "1"
      assertEval "(or #f #f 0 #f)" "0"
      assertEval "(or 1 #t)" "1"
      assertEval "(or (= 2 2) (> 2 1))" "#t"
      assertEval "(or (= 2 2) (< 2 1))" "#t"
      assertEval "(or #f #f #f)" "#f"

    it "set!" $ do
      assertEval "(define f 5) (set! f 2) f" "2"
      assertEvalErr "(set! f 2)"

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

    it "anonymous functions" $ do
      assertFile "lambda.scm" (Number 123)

    it "scoped assignment" $ do
      assertFile "assign.scm" (Number 7)

    it "ignores comments" $ do
      assertFile "comments.scm" (Number 3)

    it "varargs" $ do
      assertFile "varargs.scm" (List [Number 34, Number 1, Number 4, Number 5] mockLoc)

    it "reads scheme-files" $ do
      assertFile "read.scm" (List [Number 2, Number 3, Number 4] mockLoc)

assertFile :: (HasCallStack) => String -> LispVal -> Expectation
assertFile file expected = do
  contents <- liftIO $ readFile ("fixtures/" ++ file)
  actual <- run contents file
  actual `shouldBe` Right expected

assertParse :: (HasCallStack) => String -> [LispVal] -> Expectation
assertParse exprs expected =
  readExprs exprs "file" `shouldBe` Right expected

assertParseErr :: (HasCallStack) => String -> Expectation
assertParseErr expr =
  readExprs expr "file" `shouldSatisfyWithMessage` isLeft

assertEval :: (HasCallStack) => String -> String -> Expectation
assertEval expr expected = do
  actual <- run expr "file"
  case actual of
    Left _ -> error "eval error"
    Right actual -> show actual `shouldBe` expected

assertEvalErr :: (HasCallStack) => String -> Expectation
assertEvalErr expr = do
  actual <- run expr "file"
  actual `shouldSatisfyWithMessage` isLeft

shouldSatisfyWithMessage :: (Show a) => a -> (a -> Bool) -> Expectation
shouldSatisfyWithMessage actual predicate =
  if predicate actual
    then return () -- Test passes
    else expectationFailure $ "Predicate failed on: " ++ show actual -- Print the actual value
