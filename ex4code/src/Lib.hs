module Lib
  ( Token(..)
  , Op(..)
  , takeWhile
  , dropWhile
  , break
  , splitOn
  , lex
  , tokenize
  , interpret
  , shunt
  ) where

import           Data.Char (isDigit)
import           Prelude   hiding (break, dropWhile, lex, takeWhile)

takeWhile, dropWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs)
  | p x = x : takeWhile p xs
  | otherwise = takeWhile p xs

dropWhile _ [] = []
dropWhile p (x:xs)
  | p x = dropWhile p xs
  | otherwise = x : dropWhile p xs

break :: (a -> Bool) -> [a] -> ([a], [a])
break p = span (not . p)

--break p (x:xs) =
--  | p x = ([], xs)
--  | otherwise = (
--break p xs = foldl (\acc curr -> if p curr then else) ([],[]) xs
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn = undefined

data Token
  = TokOp Op
  | TokInt Int
  | TokErr
  deriving (Eq, Show)

data Op
  = Plus
  | Minus
  | Div
  | Mult
  deriving (Show, Eq)

lex :: String -> [String]
lex = undefined

tokenize :: [String] -> [Token]
tokenize = undefined

interpret :: [Token] -> [Token]
interpret = undefined

opLeq :: Token -> Token -> Bool
opLeq = undefined

shunt :: [Token] -> [Token]
shunt = undefined

shuntInternal :: [Token] -> [Token] -> [Token] -> [Token]
shuntInternal = undefined
