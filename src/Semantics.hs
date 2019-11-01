module Semantics where

import           Lambda
import           Control.Applicative            ( (<|>) )
import qualified Parser                        as P

substitute :: Expr -> String -> Expr -> Expr
substitute (App exp1 exp2) s e =
  eval $ App (substitute exp1 s e) (substitute exp2 s e)
substitute (Lam s exp1) s' exp2 | s == s'   = Lam s' (substitute exp1 s' exp2)
                                | otherwise = Lam s (substitute exp1 s' exp2)
substitute (Var s) s' exp | s == s'   = exp
                          | otherwise = Var s

eval :: Expr -> Expr
eval (Let s exp1 exp2) = substitute (eval exp2) s (eval exp1)
eval (App exp1 exp2  ) = case eval exp1 of
  Lam s exp1 -> substitute exp1 s (eval exp2)
  Var s      -> App (Var s) (eval exp2)
  App _ _    -> App (eval exp1) (eval exp2)
eval (Lam s exp) = Lam s (eval exp)
eval x           = x

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
