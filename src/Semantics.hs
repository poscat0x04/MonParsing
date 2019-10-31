module Semantics where

import           Lambda

subsitute :: Expr -> String -> Expr -> Expr
subsitute (App exp1 exp2) s e = App (subsitute exp1 s e) (subsitute exp2 s e)
subsitute (Lam s exp1) s' exp2 = Lam s (subsitute exp1 s' exp2)
subsitute (Var s) s' exp | s == s'   = exp
                         | otherwise = Var s

eval :: Expr -> Expr
eval (App exp1 exp2) = case exp1 of
                          Lam s exp1 -> eval $ subsitute exp1 s exp2
                          Var s -> error "applying to a variable"
                          App _ _ -> eval (App (eval exp1) exp2)
eval x = x
