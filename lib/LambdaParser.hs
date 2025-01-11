module LambdaParser where

import ParserCombinators

data Expr = App Expr Expr
  | Lam String Expr
  | Let String Expr Expr
  | Var String

expr :: Parser Expr
expr = atom `chainl1` (return App)

atom = lam +++ local +++ var +++ paren

lam = do
  _ <- symbol "\\"
  binding <- variable
  _ <- symbol "->"
  body <- expr
  return (Lam binding body)

local = do
  _ <- symbol "let"
  binding <- variable
  _ <- symbol "="
  value <- expr
  _ <- symbol "in"
  body <- expr
  return (Let binding value body)

var = do
  x <- variable
  return (Var x)

paren = bracket (symbol "(") expr (symbol ")")

variable = identifier ["let", "in"]
  

