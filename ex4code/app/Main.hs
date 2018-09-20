module Main where

import           Control.Monad
import           Lib
import           System.Environment

parseInfix :: String -> [Token]
parseInfix = interpret . shunt . tokenize . Lib.lex

main :: IO ()
main = forever $ getLine >>= print . parseInfix
