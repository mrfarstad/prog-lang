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
  | otherwise = []

dropWhile _ [] = []
dropWhile p lst@(x:xs)
  | p x = dropWhile p xs
  | otherwise = lst

break :: (a -> Bool) -> [a] -> ([a], [a])
break p xs = (takeWhile (not . p) xs, dropWhile (not . p) xs)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn s xs
  | length res > 0 = [res] ++ splitOn s tail
  | otherwise = []
  where
    p = (== s)
    lst = dropWhile p xs
    (res, tail) = break p lst

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
lex = splitOn ' '

isAllDigits :: String -> Bool
isAllDigits (x:xs) =
  isDigit x &&
  if length xs > 0
    then isAllDigits xs
    else True

makeToken :: String -> Token
makeToken str
  | str == "+" = TokOp Plus
  | str == "-" = TokOp Minus
  | str == "*" = TokOp Mult
  | str == "/" = TokOp Div
  | isAllDigits str = TokInt (read str :: Int)
  | otherwise = TokErr
  where
    int = (read str :: Int)

tokenize :: [String] -> [Token]
tokenize = map makeToken

interpret :: [Token] -> [Token]
interpret = foldl foldingFunction []
  where
    foldingFunction (TokInt x:TokInt y:ys) (TokOp Plus) = (TokInt $ x + y) : ys
    foldingFunction (TokInt x:TokInt y:ys) (TokOp Minus) = (TokInt $ y - x) : ys
    foldingFunction (TokInt x:TokInt y:ys) (TokOp Mult) = (TokInt $ x * y) : ys
    foldingFunction (TokInt x:TokInt y:ys) (TokOp Div) =
      (TokInt $ y `div` x) : ys
    foldingFunction xs (TokInt int) = TokInt int : xs
    foldingFunction xs (TokErr) = [TokErr]

opLeq :: Token -> Token -> Bool
opLeq = undefined

shunt :: [Token] -> [Token]
shunt = undefined

shuntInternal :: [Token] -> [Token] -> [Token] -> [Token]
shuntInternal = undefined
