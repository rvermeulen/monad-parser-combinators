module OnsideLambdaExprParserTest where

import OnsideLambdaExprParser
import OnsideParser
import Test.HUnit

onside_example :: Test
onside_example = "onside_example" ~: (runParser (many1 (symbol "a"))) ((0,0)," a\n a\n a") ~?= Just ["a", "a", "a"]

var_example :: Test
var_example = "var_example" ~: (runParser expr) ((0,0), "x") ~?= Just (Var "x")

lambda_example :: Test
lambda_example = "lambda_example" ~: (runParser expr) ((0,0), "(\\x -> x)") ~?= Just (Lam "x" (Var "x"))


let_example :: Test
let_example = "let_example" ~: (runParser expr) ((0,0), "let x = y in z x") ~?= Just (Let [("x",Var "y")] (App (Var "z") (Var "x")))

let_lambda_example :: Test
let_lambda_example = "let_lambda_example" ~: (runParser expr) ((0,0), "let x = \\x -> x in x y") ~?= Just (Let [("x", Lam "x" (Var "x"))] (App (Var "x") (Var "y")))

let_example_prog :: String
let_example_prog = "let foo = (\\x -> x)\n\
                   \    bar = (\\x -> x x)\n\
                   \    baz = (\\x -> x x x)\n\
                   \in\n\
                   \foo y"
onside_let_example :: Test
onside_let_example = "onside_let_example" ~: (runParser expr) ((0,0), let_example_prog) ~?= Just (Let [("foo",Lam "x" (Var "x")),("bar",Lam "x" (App (Var "x") (Var "x"))),("baz",Lam "x" (App (App (Var "x") (Var "x")) (Var "x")))] (App (Var "foo") (Var "y")))

onside_embedded_let_example :: Test
onside_embedded_let_example = "onside_embedded_let_example" ~: (runParser expr) ((0,0),
    "let foo = (\\x -> x)\n\
    \    bar = (\\x -> x x)\n\
    \in\n\
    \    let baz = (\\x -> x x x)\n\
    \    in\n\
    \    baz bar foo") ~?= Just (Let [("foo",Lam "x" (Var "x")),("bar",Lam "x" (App (Var "x") (Var "x")))] (Let [("baz",Lam "x" (App (App (Var "x") (Var "x")) (Var "x")))] (App (App (Var "baz") (Var "bar")) (Var "foo"))))

tests :: [Test]
tests = [onside_let_example, var_example, let_example, lambda_example, let_lambda_example, onside_example, onside_embedded_let_example]

