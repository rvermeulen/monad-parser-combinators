{-# LANGUAGE FunctionalDependencies #-}

module MonadicParserCombinators where

{-class Monad m => StateMonad s m where
  update :: (s -> s) -> m s

  set :: s -> m s
  set s = update (\_ -> s)

  fetch :: m s
  fetch = update (\s -> s)-}

class Monad m => MonadPlus m where
  mzero :: m a

  mplus :: m a -> m a -> m a

class Applicative f => Alternative f where
  empty :: f a
  
  (<|>) :: f a -> f a -> f a

newtype StateT s m a = StateT { runStateT :: s -> m (a, s)}

instance Functor m => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f m = StateT $ \s -> fmap (\(v, s') -> (f v, s')) $ runStateT m s

instance (Functor m, Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)

  (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  mf <*> mx = StateT $ \s -> do
    (f, s') <- runStateT mf s
    (x, s'') <- runStateT mx s'
    return (f x, s'')

instance (Alternative m, MonadPlus m) => Alternative (StateT s m ) where
  empty = mzero

  p <|> p' = StateT $ \s -> runStateT p s <|> runStateT p' s
  
instance (Monad m) => Monad (StateT s m) where
  (>>=) :: (StateT s m a) -> (a -> StateT s m b) -> StateT s m b
  m >>= f = StateT $ \s -> do
    (v, s') <- runStateT m s
    runStateT (f v) s'

instance (MonadPlus m) => MonadPlus (StateT s m) where
  mzero :: StateT s m a
  mzero = StateT $ \_ -> mzero

  mplus :: StateT s m a -> StateT s m a -> StateT s m a
  m `mplus` n = StateT $ \s -> runStateT m s `mplus` runStateT n s

instance Alternative [] where
  empty = []
  -- To make our parser determinisitc, we only return the first succeeding parser
  m <|> n = case m ++ n of
    [] -> []
    x:_ -> [x]

instance MonadPlus [] where
  mzero = empty
  mplus = (<|>)

newtype Identity a = Identity { runIdentity :: a}

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f m = Identity $ f $ runIdentity m

type State s = StateT s Identity

state :: (Monad m) => (s -> (a, s)) -> StateT s m a
state f = StateT (return . f)

runState :: State s a -> s -> (a, s)
-- runStateT st -> Identity \s -> (a, s) -> \s -> (a, s)
runState st = runIdentity . runStateT st

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()
  modify :: (s -> s) -> m ()

instance Monad m => MonadState s (StateT s m) where
    get = state $ \s -> (s, s)
    put s = state $ \_ -> ((), s)
    modify f = state $ \s -> ((), f s)

{-
  The type [] forms a monad with a zero and a plus, therefore so does
  State [] String.
  This gives us:

  pure     :: a -> Parser a
  (>>=)    :: Parser a -> (a -> Parser a) -> Parser a
  mzero    :: Parser a
  mplus    :: Parser a -> Parser a -> Parser a
-}
type Parser a = StateT String [] a

parser :: (Monad m) => (s -> (a, s)) -> StateT s m a
parser f = StateT (return . f)

runParser :: Parser a -> String -> [(a, String)]
runParser = runStateT

item :: Parser Char
{-item = parser $ \inp -> case inp of
  [] -> mzero
  (x:xs) -> return (x:xs)
-}

{-
   The monadic implemention of item that updates the current state,
   that is our input string, to the tail of the input string and uses
   the head of the old state to get the next character.
-}
item = do
  x <- get
  modify tail
  return (head x)

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x
    then return x
    else mzero

char :: Char -> Parser Char
char x = sat (\ch -> ch == x)

digit :: Parser Char
digit = sat (\ch -> '0' <= ch && ch <= '9')

lower :: Parser Char
lower = sat (\ch -> 'a' <= ch && ch <= 'z')

upper :: Parser Char
upper = sat (\ch -> 'A' <= ch && ch <= 'Z')

string :: String -> Parser String
string s =
  do
    _ <- char (head s)
    _ <- string (tail s)
    return s

many :: Parser a -> Parser [a]
many p =
  do
    x <- p
    xs <- many p
    return (x : xs)
  <|>
  empty

many1 :: Parser a -> Parser [a]
many1 p =
  do
    x <- p
    xs <- many p
    return (x : xs)

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
