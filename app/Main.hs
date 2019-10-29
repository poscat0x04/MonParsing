{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Data.Char

main :: IO ()
main = print . head $ runParser expr "1+2-3-4"
