module OnsideParser where

--import Debug.Trace

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  f `fmap` st = State $ \s -> let (a, s') = (runState st) s in (f a, s')
  
instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State $ \s -> (a, s)
  
instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  st >>= f = State $ \s -> let (a, s') = runState st s in runState (f a) s'

class Monad m => StateMonad m s where
  update :: (s -> s) -> m s
  set :: s -> m s
  fetch :: m s

  set s = update (\_ -> s)
  fetch = update id

class Monad m => MonadOPlus m where
  zero :: m a
  mplus :: m a -> m a -> m a

instance StateMonad (State s) s where
  update :: (s -> s) -> State s s
  update f = State $ \s -> (s, f s)

newtype StateM m s a = StateM { runStateM :: s -> m (a, s) }

instance Functor m => Functor (StateM m s) where
  fmap :: (a -> b) -> StateM m s a -> StateM m s b
  f `fmap` st = StateM $ \s -> fmap (\(v , s') -> (f v, s')) $ runStateM st s
  
instance Applicative m => Applicative (StateM m s) where
  pure :: a -> StateM m s a
  pure a = StateM $ \s -> pure (a, s)

instance Monad m => Monad (StateM m s) where
  (>>=) :: StateM m s a -> (a -> StateM m s b) -> StateM m s b
  stm >>= f = StateM $ \s -> do
    (a, s') <- runStateM stm s
    runStateM (f a) s'

instance MonadOPlus m => MonadOPlus (StateM m s) where
  zero = StateM $ \s -> zero

  stm `mplus` stm' = StateM $ \s -> runStateM stm s `mplus` runStateM stm' s

instance MonadOPlus [] where
  zero = empty
  mplus = (<|>)

instance Monad m => StateMonad (StateM m s) s where
  update f = StateM $ \s -> return (s, f s)

instance MonadFail (StateM [] String) where
  fail :: String -> StateM [] String a
  fail _ = zero

newtype ReaderM m s a = ReaderM { runReaderM :: s -> m a }

instance Functor m => Functor (ReaderM m s) where
  fmap :: (a -> b) -> ReaderM m s a -> ReaderM m s b
  f `fmap` rm = ReaderM $ \s -> fmap f (runReaderM rm s)
  
instance Applicative m => Applicative (ReaderM m s) where
  pure :: a -> ReaderM m s a
  pure a = ReaderM $ \_ -> pure a
  
instance Monad m => Monad (ReaderM m s) where
  (>>=) :: ReaderM m s a -> (a -> ReaderM m s b) -> ReaderM m s b
  rm >>= f = ReaderM $ \s ->
    do
      a <- runReaderM rm s
      runReaderM (f a) s

instance MonadOPlus m => MonadOPlus (ReaderM m s) where
  zero :: ReaderM m s a
  zero = ReaderM $ \_ -> zero

  mplus :: ReaderM m s a -> ReaderM m s a -> ReaderM m s a
  rm `mplus` rm' = ReaderM $ \s -> runReaderM rm s `mplus` runReaderM rm' s

class Monad m => ReaderMonad m s where
  env :: m s
  setenv :: s -> m a -> m a

instance (Monad m, Show s) => ReaderMonad (ReaderM m s) s where
  env :: Monad m => ReaderM m s s
  env = ReaderM $ \s -> pure s

  setenv :: Monad m => s -> ReaderM m s a -> ReaderM m s a
  setenv s rm = ReaderM $ \_ -> {-trace ("setenv: " ++ show s)-} (runReaderM rm s)


instance StateMonad m a => StateMonad (ReaderM m s) a where
  update f = ReaderM $ \_ -> update f

type Parser a = ReaderM (StateM [] PString) Pos a
type PString = (Pos, String)
type Pos = (Int, Int)

class Applicative m => Alternative m where
  empty :: m a

  (<|>) :: m a -> m a -> m a

instance Alternative m => Alternative (ReaderM m s) where
  empty :: ReaderM m s a
  empty = ReaderM $ \_ -> empty

  (<|>) :: ReaderM m s a -> ReaderM m s a -> ReaderM m s a
  rm <|> rm' = ReaderM $ \s -> runReaderM rm s <|> runReaderM rm' s

instance Alternative m => Alternative (StateM m s) where
  empty :: StateM m s a
  empty = StateM $ \_ -> empty

  (<|>) :: StateM m s a -> StateM m s a -> StateM m s a
  sm <|> sm' = StateM $ \s -> runStateM sm s <|> runStateM sm' s

instance Alternative [] where
  empty = []

  lhs <|> rhs = case (lhs ++ rhs) of
    [] -> []
    (x:_) -> [x]

instance MonadFail m => MonadFail (ReaderM m s) where
  fail :: String -> ReaderM m s a
  fail s = ReaderM $ \_ -> fail s

instance MonadFail m => MonadFail (StateM m s) where
  fail :: String -> StateM m s a
  fail s = StateM $ \_ -> fail s

guard :: Alternative m => Bool -> m ()
guard False = empty
guard True = pure ()

item :: Parser Char
item = do
  --traceM "item"
  (pos, x:_) <- update newstate
  --traceM ("pos:" ++ show pos ++ " x:" ++ show x)
  defpos <- env
  --traceM ("def:" ++ show defpos)
  --traceM ("onside:" ++ show (onside pos defpos))
  guard (onside pos defpos)
  return x

onside :: Pos -> Pos -> Bool
onside (l, c) (dl, dc) = c > dc || l == dl

newstate :: PString -> PString
newstate ((l, c), x:xs) =
  {-trace ("newstate: " ++ show newpos ++ " '" ++ xs ++ "'")-}
  (newpos, xs)
  where
    newpos = case x of
      '\n' -> (l+1, 0)
      '\t' -> (l, ((c `div` 8) + 1)*8)
      _ -> (l, c + 1)
newstate (pos, []) = (pos, [])

sat :: (Char -> Bool) -> Parser Char
sat p = do
  ch <- item
  --traceM ("sat:" ++ show (p ch))
  guard(p ch)
  return ch

many :: Parser a -> Parser [a]
many p =
  do
    --traceM "many"
    x <- p
    xs <- many p
    return (x:xs)
  <|>
  do
    --traceM "many failed"
    return zero

many1 :: Parser a -> Parser [a]
many1 p =
  do
    --traceM ("many1")
    x <- p
    xs <- many p
    return (x:xs)

spaces :: Parser ()
spaces = do
  --traceM "spaces"
  _ <- many1 (sat isSpace)
  return ()
  where isSpace x = (x == ' ') || (x == '\n') || (x == '\t')

comment :: Parser ()
comment = do
  --traceM ("comment")
  _ <- string "--"
  _ <- many (sat (\ch -> ch == '\n'))
  return()

char :: Char -> Parser Char
char x = do
  _ <- sat (\x' -> x == x')
  return x

string :: String -> Parser String
string "" = return ""
string (x:xs) = do
  --traceM ("string: '" ++ (x:xs) ++ "'")
  _ <- char x
  _ <- string xs
  return (x:xs)

junk :: Parser ()
junk = do
  --traceM "junk"
  -- Ensure we are always onside by setting the dc to -1
  _ <- setenv ((0 , -1) :: Pos) (many (spaces <|> comment))
  return ()

many1_offside :: Parser a -> Parser [a]
many1_offside p = do
  --traceM "many1_offside"
  (pos, _) :: PString <- fetch
  vs <- setenv pos (many1 (off p))
  return vs

off :: Parser a -> Parser a
off p = do
  (_ , dc) :: Pos <- env
  ((l, c), _ ) :: PString <- fetch
  --traceM ("off:" ++ show (c == dc))
  guard (c == dc) -- one the same column as the definition position
  v <- setenv (l, dc) p
  return v

many_offside :: Parser a -> Parser [a]
many_offside p = many1_offside p <|> return []

digit :: Parser Char
digit = sat (\ch -> '0' <= ch && ch <= '9')

lower :: Parser Char
lower = sat (\ch -> 'a' <= ch && ch <= 'z')

upper :: Parser Char
upper = sat (\ch -> 'A' <= ch && ch <= 'Z')

letter :: Parser Char
letter = lower <|> upper

alphanum :: Parser Char
alphanum = letter <|> digit

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x : xs)

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

symbol :: String -> Parser String
symbol xs = do
  --traceM ("symbol: '" ++ xs ++ "'")
  token (string xs)

identifier :: [String] -> Parser String
identifier keywords = token
  (do
    x <- ident
    guard(not (elem x keywords))
    return x)

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do
  _ <- open
  x <- p
  _ <- close
  return x

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
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

runParser :: Parser a -> PString -> Maybe a
runParser p s =
  let (pos, _) = s in
  -- Extra newline to start printing under the test case summary
  --trace ("\nrunParser on:" ++ show s) (
  case runStateM (runReaderM (parse p) pos) s of
                [] -> Nothing
                [(res, _)] -> Just res
                ((res, _):_) -> Just res--)

