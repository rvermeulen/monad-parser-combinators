module Main (main) where

import ParserCombinators
import Test.HUnit
import qualified System.Exit as Exit
import qualified ExprParserTest
import qualified MonadicParserCombinatorsTest
import qualified OnsideLambdaExprParserTest

upper_test :: Test
upper_test = "upper_test" ~: (runParser upper "Hello") ~?= [('H', "ello")]

word_test :: Test
word_test = "word_test" ~: (runParser word "Yes!") ~?= [("Yes","!"),("Ye","s!"),("Y","es!"),("","Yes!")]

string_test :: Test
string_test = "string_test" ~: (runParser (string "hello") "hello there") ~?= [("hello", " there")]

many_test :: Test
many_test = "many_test" ~: (runParser (many letter) "Yes!") ~?= [("Yes","!"),("Ye","s!"),("Y","es!"),("","Yes!")]

pos_int_test :: Test
pos_int_test = "pos_int_test" ~: (runParser int "100") ~?= [(100,""),(10,"0"),(1,"00")]

neg_int_test :: Test
neg_int_test = "neg_int_test" ~: (runParser int "-100") ~?= [(-100,""),(-10,"0"),(-1,"00")]

pos_ints_test :: Test
pos_ints_test = "pos_ints_test" ~: (runParser ints "[1,2,3]") ~?= [([1,2,3],"")]

tests :: Test
tests = TestList ([
  upper_test,
  word_test,
  string_test,
  many_test,
  pos_int_test,
  neg_int_test,
  pos_ints_test
  ] ++ ExprParserTest.tests ++ MonadicParserCombinatorsTest.tests ++ OnsideLambdaExprParserTest.tests)

--tests = TestList [OnsideLambdaExprParserTest.onside_let_example]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
