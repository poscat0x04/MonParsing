module Semantics where

import           Lambda
import           Control.Applicative            ( (<|>) )
import qualified Parser                        as P

substitute :: Expr -> String -> Expr -> Expr
substitute (App exp1 exp2) s e = App (substitute exp1 s e) (substitute exp2 s e)
substitute (Lam s exp1) s' exp2 | s == s'   = error "Variable naming conflict"
                               | otherwise = Lam s (substitute exp1 s' exp2)
substitute (Var s) s' exp | s == s'   = exp
                         | otherwise = Var s

eval :: Expr -> Expr
eval (Let s exp1 exp2) = eval $ substitute exp2 s exp1
eval (App exp1 exp2  ) = case exp1 of
  Lam s exp1 -> eval $ substitute exp1 s exp2
  Var s      -> error "Applying a variable"
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
