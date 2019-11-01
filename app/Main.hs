{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import           Parser
import           Data.Char
import           System.Console.Haskeline
import           Control.Monad.Trans
import qualified Lambda
import qualified Semantics

main :: IO ()
main = runInputT defaultSettings loop
  where loop = do
            command <- getInputLine "> "
            outputStrLn $ "result: " ++ show case command >>= \s -> runParser Lambda.expr s of
                                      Just (e, _) -> Semantics.eval e
                                      Nothing     -> error "Parse failed"
                                    
            loop
