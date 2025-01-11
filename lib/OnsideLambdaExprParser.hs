module OnsideLambdaExprParser where

--import Debug.Trace
import OnsideParser

data Expr = App Expr Expr
  | Lam String Expr
  | Let [(String, Expr)] Expr
  | Var String
  deriving (Show, Eq)

expr :: Parser Expr
expr = atom `chainl1` (return App)

atom = lam <|> local <|> var <|> paren

lam = do
  --traceM "lam"
  _ <- symbol "\\"
  binding <- variable
  _ <- symbol "->"
  body <- expr
  return (Lam binding body)

local = do
  --traceM "local"  
  -- capture the position we expect the let to be to allow in to start in the same column
  ((let_l, let_c), _) :: PString <- fetch
  _ <- symbol "let"
  --traceM ("Let starts at " ++ (show (let_l, let_c)))
  ds <- many1_offside defn
  ((in_l, _), _) :: PString <- fetch
  --traceM ("In could start at " ++ (show (in_l, let_c)))
  _ <- setenv (in_l, let_c) (symbol "in")
  ((body_l, _), _) :: PString <- fetch
  body <- setenv (body_l, let_c) expr
  return (Let ds body)

defn = do
  --traceM "defn"
  x <- variable
  _ <- symbol "="
  e <- expr
  return (x, e)

var = do
  --traceM "var"
  x <- variable
  return (Var x)

paren = {-trace "paren"-} bracket (symbol "(") expr (symbol ")")

variable = {-trace "variable"-} identifier ["let", "in"]
  

