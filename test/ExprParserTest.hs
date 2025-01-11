module ExprParserTest where

import ExprParser
import ParserCombinators
import Test.HUnit

simple_add :: Test
simple_add = "simple_add" ~: (runParser expr) "1+1" ~?= [(2, ""), (1, "+1")]

paper_example :: Test
paper_example =
  "paper_example"
    ~: (runParser expr) "1+2-(3+4)"
    ~?= [(-4, ""), (3, "-(3+4)"), (1, "+2-(3+4)")]

exp_test :: Test
exp_test = "exp_test" ~: (runParser expr) "2^1+2^2^3-2" ~?= [(256,""),(258,"-2"),(6,"^3-2"),(4,"^2^3-2"),(2,"+2^2^3-2"),(2,"^1+2^2^3-2")]

tests :: [Test]
tests = [simple_add, paper_example, exp_test]
