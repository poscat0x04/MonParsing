{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Parser
import           Data.Char
import qualified Lambda
import qualified Semantics

main :: IO ()
main = do
  putStr ">\n"
  command <- getLine
  putStrLn $ "result: " ++ show
    (case runParser Lambda.expr command of
      Just (e, _) -> Semantics.eval e
      Nothing     -> error "Parse failed"
    )
  main
