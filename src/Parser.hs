{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonadComprehensions #-}

module Parser
  ( Parser
  , sat
  , runParser
  , word
  , alphanum
  , letter
  , many
  , nat
  , int
  , ints
  , expr
  )
where

import           Control.Applicative            ( Alternative
                                                , empty
                                                , (<|>)
                                                )
import           Control.Monad
import           Data.Char

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

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
  empty = Parser (const [])
  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser (\inp -> runParser p1 inp ++ runParser p2 inp)

instance MonadPlus Parser

item :: Parser Char
item = Parser
  (\case
    []       -> []
    (x : xs) -> return (x, xs)
  )

sat :: (Char -> Bool) -> Parser Char
sat pred = do
  x <- item
  guard (pred x)
  return x

char :: Char -> Parser Char
char ch = sat (== ch)

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

expr :: Parser Int
expr =
  do
      x <- factor
      f <- addop
      y <- expr
      return (f x y)
    <|> factor

addop :: Parser (Int -> Int -> Int)
addop =
  do
      _ <- char '+'
      return (+)
    <|> do
          _ <- char '-'
          return (-)

factor :: Parser Int
factor = nat <|> bracket (char '(') expr (char ')')
