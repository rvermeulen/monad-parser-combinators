module ParserCombinators where

import Control.Applicative (Alternative, (<|>), empty)
import Control.Monad (MonadPlus, mplus, mzero)
import Data.Char (ord)

--import Debug.Trace
-- Non-deterministic parser, returns zero or more results
-- Zero signals failure
newtype Parser a = Parser
  { runParser :: String -> [(a, String)]
  }

parser :: (String -> [(a, String)]) -> Parser a
parser = Parser

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  f `fmap` p =
    parser $ \inp -> map (\(v, inp') -> (f v, inp')) (runParser p inp)

instance Applicative Parser where
  pure :: a -> Parser a
  pure v = parser $ \inv -> [(v, inv)]
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p <*> q = do
    f <- p
    a <- q
    return (f a)

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    parser $ \inp -> do
      (v, out) <- runParser p inp
      runParser (f v) out

instance Alternative Parser where
  empty :: Parser a
  empty = parser $ \_ -> []
  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = parser $ \inp -> (runParser p inp) ++ (runParser q inp)

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)

-- Basic combinators
seq :: Parser a -> Parser b -> Parser (a, b)
p `seq` q = do
  v <- p
  w <- q
  return (v, w)

item :: Parser Char
item =
  parser $ \inp ->
    case inp of
      [] -> []
      (x:xs) -> [(x, xs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x
    then return x
    else empty

char :: Char -> Parser Char
char x = sat (\ch -> ch == x)

digit :: Parser Char
digit = sat (\ch -> '0' <= ch && ch <= '9')

lower :: Parser Char
lower = sat (\ch -> 'a' <= ch && ch <= 'z')

upper :: Parser Char
upper = sat (\ch -> 'A' <= ch && ch <= 'Z')

plus :: Parser a -> Parser a -> Parser a
p `plus` q = parser $ \inp -> (runParser p inp) ++ (runParser q inp)

letter :: Parser Char
letter = lower `plus` upper

alphanum :: Parser Char
alphanum = letter `plus` digit

word :: Parser String
{-word = neWord `plus` pure ""
       where neWord = do
               x <- letter
               xs <- word
               return (x:xs)-}
word = many letter

string :: String -> Parser String
string "" = return ""
string (x:xs) = do
  _ <- char x
  _ <- string xs
  return (x : xs)

many :: Parser a -> Parser [a]
many p =
  (do
     x <- p
     xs <- many p
     return (x : xs))
    <|> return []

many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x : xs)

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

nat :: Parser Int
{-nat = do
  xs <- many1 digit
  return (eval xs)
  where
    eval xs = foldl1 op [ord x - ord '0' | x <- xs]
    m `op` n = m * 10 + n-}
nat = (do
  x <- digit
  return (ord x - ord '0'))
  `chainl1`
  return op
  where
    m `op` n = 10*m + n
  

int :: Parser Int
int = do
  f <- op
  n <- nat
  return (f n)
  where
    op =
      (do
         _ <- char '-'
         return negate)
        <|> pure id

ints :: Parser [Int]
{-ints = do
  _ <- char '['
  n <- int
  ns <- many comma_int
  _ <- char ']'
  return (n : ns)
  where
    comma_int = do
      _ <- char ','
      n <- int
      return n-}
{-ints = do
  _ <- char '['
  xs <- sepby1 int (char ',')
  _ <- char ']'
  return xs-}
ints = bracket (char '[') (sepby1 int (char ',')) (char ']')

sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 p sep = do
  x <- p
  xs <- many sep_then_p
  return (x : xs)
  where
    sep_then_p =
      (do
         _ <- sep
         y <- p
         return y)

sepby :: Parser a -> Parser b -> Parser [a]
sepby p sep = (sepby1 p sep) <|> return []

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do
  _ <- open
  x <- p
  _ <- close
  return x

{-
  The chain operations employ left-factorization which prevent uncessary
  backtracking and results in linear time parsing.

  An example that leads to unnecessary backtracking is:

  eval :: Parser Int
  eval = add <|> sub
         where
           add = do
                   x <- nat
                   _ <- char '+'
                   y <- nat
                   return (x + y)
           sub = do
                   x <- nat
                   _ <- char '-'
                   y <- nat
                   return (x - y)

  The expression 123-123 leads to unnecessary re-parsing of the first nat 123
  when first add is called, followed by sub.
-}

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
{-}chainl1 p op =
  do
    x <- p
    fys <- many op_and_p
    return (run x fys)
    where
      op_and_p =
        do
          f <- op
          y <- p
          return (f, y)
      run :: a -> [(a -> a -> a, a)] -> a
      run x fys = foldl (\acc (f, y) -> f acc y) x fys-}
chainl1 p op =
  do
    x <- p
    fys <- rest x
    return fys
    where
      rest x =
        (do
          f <- op
          y <- p
          rest (f x y))
        <|>
        return x
  
ops :: [(Parser a, b)] -> Parser b
ops xs = foldr1 (<|>) (map (\(p, op) -> (do
                              _ <- p
                              return op
                          )) xs)

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainr1 p op =
  do
    x <- p
    f <- op
    y <- p `chainr1` op
    return (f x y)
  <|>
  do
    x <- p
    return x
   
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op v =
  p `chainl1` op
  <|>
  return v

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op v =
  p `chainr1` op
  <|>
  return v

{-
   Given a parser p that always succeeds, force will have the same behavior, but
   before any attempt of parsing the input string is attempted the result of the
   parser is immediately forced to take on the form (_, _) : _ where _ represents
   a presently undefined value.

   This can be used to make parser like many truely lazy.

   Is this still necessary?
   How to find out?

   For now we won't use it.
-}
force :: Parser a -> Parser a
force p = parser $ \inp -> let x = runParser p inp in
  (fst (head x), snd (head x)) : tail x

first :: Parser a -> Parser a
first p = parser $ \inp -> case runParser p inp of
  [] -> []
  (x:xs) -> [x]

-- Only return the first results, i.e., make parsing deterministic.
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = first (p <|> q)

spaces :: Parser ()
spaces = do
  _ <- many1 (sat isSpace)
  return ()
  where
    isSpace x = (x == ' ') || (x == '\n') || (x == '\t')

comment :: Parser ()
comment = do
  _ <- string "--"
  _ <- many (sat (\x -> x /= '\n'))
  return ()

junk :: Parser ()
junk = do
  _ <- many (spaces +++ comment)
  return ()

-- parse ensures parser p starts at a significant character
parse :: Parser a -> Parser a
parse p = do
   _ <- junk
   v <- p
   return v

-- token ensures a token ends at a significant character
token :: Parser a -> Parser a
token p = do
  v <- p
  _ <- junk
  return v

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

identifier :: [String] -> Parser String
identifier keywords = token
  (do
    x <- ident
    if not (elem x keywords) then return x else empty)
