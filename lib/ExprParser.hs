module ExprParser where

import Control.Applicative ((<|>))
import ParserCombinators

{-
  expr    ::= expr addop factor | factor
  addop   ::= '+' | '-'
  factor  ::= nat | '(' expr ')'
-}
expr :: Parser Int
{-expr = do
  x <- expr -- left-recursive will result in infinite loop
  f <- addop
  y <- factor
  return (f x y) <|> factor-}
{-expr = do
  x <- factor
  fys <- many addop_and_factor
  return (eval x fys)
  where
    addop_and_factor :: Parser (Int -> Int -> Int, Int)
    addop_and_factor = do
      f <- addop
      y <- factor
      return (f, y)
    eval :: Int -> [(Int -> Int -> Int, Int)] -> Int
    eval x fys = foldl (\acc (op, y) -> op acc y) x fys-}
expr = term `chainl1` addop

term :: Parser Int
term = factor `chainr1` expop

addop :: Parser (Int -> Int -> Int)
{-addop =
  (do
     _ <- char '+'
     return (+))
    <|> (do
           _ <- char '-'
           return (-))-}
addop = ops [(char '+', (+)), (char '-', (-))]

expop :: Parser (Int -> Int -> Int)
expop = ops [(char '^', (^))]

factor :: Parser Int
factor = nat <|> (bracket (char '(') expr (char ')'))
