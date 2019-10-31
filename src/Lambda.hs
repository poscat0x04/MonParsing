module Lambda where

import           Parser                         ( chainl1
                                                , symbol
                                                , identifier
                                                , bracket
                                                )
import           Control.Applicative            ( (<|>) )


data Expr = App Expr Expr
          | Lam String Expr
          | Let String Expr Expr
          | Var String deriving (Eq)

instance Show Expr where
  show (App exp1 exp2) = "(" ++ show exp1 ++ " " ++ show exp2 ++ ")"
  show (Lam s exp) = "\\" ++ s ++ " -> " ++ show exp
  show (Let s exp1 exp2) = "let " ++ s ++ " = " ++ show exp1 ++ " in " ++ show exp2
  show (Var s) = s

expr = atom `chainl1` pure App

atom = lam <|> local <|> var <|> paren

lam = do
  symbol "\\"
  x <- variable
  symbol "->"
  e <- expr
  return $ Lam x e

local = do
  symbol "let"
  x  <- variable
  symbol "="
  e  <- expr
  symbol "in"
  e' <- expr
  return $ Let x e e'

var = fmap Var variable

paren = bracket (symbol "(") expr (symbol ")")

variable = identifier ["let", "in"]
