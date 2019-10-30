{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonadComprehensions #-}

module Parser
  ( Parser
  , sat
  , runParser
  , item
  , word
  , char
  , alphanum
  , string
  , letter
  , many
  , ident
  , nat
  , chainl1
  , int
  , ints
  , symbol
  , expr
  , seqParse
  , spaces
  , comment
  , mlcomment
  , junk
  , parse
  , token
  , identifier
  , bracket
  )
where

import           Control.Applicative            ( Alternative
                                                , empty
                                                , (<|>)
                                                )
import           Control.Monad
import           Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser
    (\inp -> do
      (a, s) <- runParser p inp
      return (f a, s)
    )

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser (\inp -> return (x, inp))
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  fp <*> p = Parser
    (\inp -> do
      (a, s ) <- runParser p inp
      (f, s') <- runParser fp s
      return (f a, s')
    )

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser
    (\inp -> do
      (a, s) <- runParser p inp
      runParser (f a) s
    )

instance Alternative Parser where
  empty :: Parser a
  empty = Parser (const mzero)
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser (\inp -> runParser p1 inp <|> runParser p2 inp)

instance MonadPlus Parser

item :: Parser Char
item = Parser
  (\case
    []       -> mzero
    (x : xs) -> return (x, xs)
  )

sat :: (Char -> Bool) -> Parser Char
sat pred = do
  x <- item
  guard (pred x)
  return x

char :: Char -> Parser Char
char ch = sat (== ch)

string :: String -> Parser String
string ""         = pure ""
string s@(x : xs) = char x >> string xs >> pure s

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

word :: Parser String
word = neWord <|> pure ""
 where
  neWord = do
    x  <- letter
    xs <- word
    return (x : xs)

many :: Parser a -> Parser [a]
many p =
  (do
      x  <- p
      xs <- many p
      return (x : xs)
    )
    <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = do
  x  <- p
  xs <- many p
  return (x : xs)

ident :: Parser String
ident = do
  x <- lower
  xs <- many alphanum
  return (x:xs)

nat :: Parser Int
nat = do
  x <- many1 digit
  return $ read x

int :: Parser Int
int =
  (do
      _ <- char '-'
      x <- nat
      return (-x)
    )
    <|> nat

ints :: Parser [Int]
ints = do
  _ <- char '['
  n <- sepby1 int $ char ','
  _ <- char ']'
  return n

sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 p sep = do
  x  <- p
  xs <- many $ do
    _ <- sep
    p
  return (x : xs)

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket leading p ending = do
  _ <- leading
  c <- p
  _ <- ending
  return c

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do
  x   <- p
  fys <- many $ do
    f <- op
    y <- p
    return (f, y)
  return $ foldl (\x (f, y) -> x `f` y) x fys

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op = do
  x <- p
  do
      f <- op
      y <- p `chainr1` op
      return (f x y)
    <|> pure x

expr :: Parser Int
expr = term `chainl1` addop

term :: Parser Int
term = factor `chainr1` expop

addop :: Parser (Int -> Int -> Int)
addop = ops [(char '-', (-)), (char '+', (+))]

expop :: Parser (Int -> Int -> Int)
expop = ops [(char '^', (^))]

factor :: Parser Int
factor = nat <|> bracket (char '(') expr (char ')')

ops :: [(Parser a, b)] -> Parser b
ops = foldr1 (<|>) . fmap (\(p, r) -> p >> return r)

seqParse :: [Parser a] -> Parser [a]
seqParse l = do
  xs <- sequence l
  return $ reverse xs

spaces :: Parser ()
spaces = many1 (sat isSpace) >> pure ()

comment :: Parser ()
comment = do
  _ <- string "--"
  _ <- many (sat (/= '\n'))
  return ()

reverseP :: Parser a -> Parser ()
reverseP p = Parser
  (\inp -> case runParser p inp of
    Nothing -> pure ((), inp)
    _       -> mzero
  )

notCommentEnd :: Parser ()
notCommentEnd = reverseP (string "-}") >> item >> pure ()

commentContent :: Parser ()
commentContent = ((mlcomment <|> notCommentEnd) >> commentContent) <|> pure ()

mlcomment :: Parser ()
mlcomment = string "{-" >> commentContent >> string "-}" >> pure ()

junk :: Parser ()
junk = many (spaces <|> comment <|> mlcomment) >> pure ()

parse :: Parser a -> Parser a
parse p = junk >> p

token :: Parser a -> Parser a
token p = do
  x <- p
  _ <- junk
  return x

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

identifier :: [String] -> Parser String
identifier ban = token $ do
  x <- ident
  guard $ x `notElem` ban
  return x
