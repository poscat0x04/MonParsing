module Semantics where

import           Lambda
import           Control.Applicative            ( (<|>) )
import qualified Parser                        as P

subsitute :: Expr -> String -> Expr -> Expr
subsitute (App exp1 exp2) s e = App (subsitute exp1 s e) (subsitute exp2 s e)
subsitute (Lam s exp1) s' exp2 | s == s'   = error "Variable conflict"
                               | otherwise = Lam s (subsitute exp1 s' exp2)
subsitute (Var s) s' exp | s == s'   = exp
                         | otherwise = Var s

eval :: Expr -> Expr
eval (Let s exp1 exp2) = eval $ subsitute exp2 s exp1
eval (App exp1 exp2  ) = case exp1 of
  Lam s exp1 -> eval $ subsitute exp1 s exp2
  Var s      -> error "applying to a variable"
  App _ _    -> eval (App (eval exp1) exp2)
eval x = x

consume :: P.Parser [Expr]
consume = P.many $ do
  P.junk
  x <- expr
  P.char ';'
  return x

run :: String -> [Expr]
run s = case P.runParser consume s of
  Just x  -> fmap eval (fst x)
  Nothing -> error "Parse failed"
