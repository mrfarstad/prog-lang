module Lib
  ( listSum
  , listProduct
  , listConcat
  , listMaximum
  , listMinimum
  , sum
  , concat
  , length
  , elem
  , safeMaximum
  , safeMinimum
  , any
  , all
  , foldr
  , Complex(..)
  ) where

import           Prelude hiding (Foldable (..), all, any, concat, elem, foldr,
                          length, maximum, minimum, product, sum)

-- TASK 2
-- Bounded parametric polymorphism
-- Implement the following functions that reduce a list to a single
-- value (or Maybe a single value).
-- Maybe is imported from Prelude and is defined like this:
-- data Maybe a = Just a | Nothing
listSum :: (Num a) => [a] -> a
listSum []     = 0
listSum (x:xs) = x + listSum xs

listProduct :: (Num a) => [a] -> a
listProduct []     = 1
listProduct (x:xs) = x * listProduct xs

listConcat :: [[a]] -> [a]
listConcat []       = []
listConcat (xs:xxs) = xs ++ listConcat (xxs)

listMaximum :: (Ord a) => [a] -> Maybe a
listMaximum [] = Nothing
listMaximum (x:xs)
  | Just x > listMaximum xs = Just x
  | otherwise = listMaximum xs

listMinimum :: (Ord a) => [a] -> Maybe a
listMinimum [] = Nothing
listMinimum [y] = Just y
listMinimum (x:xs)
  | Just x < listMinimum xs = Just x
  | otherwise = listMinimum xs

-- TASK 3 Folds
-- TASK 3.1
-- Below our Foldable class is defined. Now define a list instance of
-- Foldable, and then define the Foldable versions of the functions
-- you defined previously (and some more).
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b

instance Foldable [] where
  foldr op acc []     = acc
  foldr op acc (x:xs) = op x $ foldr op acc xs

--
-- USE FOLDR TO DEFINE THESE FUNCTIONS
--
sum :: (Num a, Foldable t) => t a -> a
sum xs = foldr (+) 0 xs

concat :: Foldable t => t [a] -> [a]
concat xxs = foldr (++) [] xxs

length :: Foldable t => t a -> Int
length xs = undefined

elem :: (Eq a, Foldable t) => a -> t a -> Bool
elem = undefined

safeMaximum :: (Foldable t, Ord a) => t a -> Maybe a
safeMaximum = undefined

safeMinimum :: (Foldable t, Ord a) => t a -> Maybe a
safeMinimum = undefined

-- The functions "any" and "all" check if any or all elements of a
-- Foldable satisfy the given predicate.
--
-- USE FOLDR
--
any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = undefined

all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = undefined

-- TASK 4
-- Num Complex
data Complex =
  Complex Double
          Double
  deriving (Eq)

instance Show Complex where
  show (Complex r i)
    | i >= 0 = show r ++ "+" ++ show i ++ "i"
    | otherwise = show r ++ "-" ++ show (abs i) ++ "i"

instance Num Complex where
  (+) (Complex r i) (Complex r1 i1) = Complex (r + r1) (i + i1)
  (*) (Complex a b) (Complex c d) = Complex (a * c - b * d) (a * d + b * c)
  abs (Complex a b) = Complex (sqrt (a * a + b * b)) 0
  signum (Complex a b) = foo
    where
      c = sqrt (a * a + b * b)
      foo = Complex (a / c) (b / c)
  fromInteger a = Complex (fromInteger a) 0
  (-) (Complex a b) (Complex c d) = Complex (a - c) (b - d)
  negate (Complex a b) = Complex (-a) (-b)

-- TASK 5
-- Making your own type classes
type Position = (Double, Double)

class Pos a where
  pos :: a -> Position

data Campus
  = Kalvskinnet
  | Gløshaugen
  | Tyholt
  | Moholt
  | Dragvoll
  deriving (Show, Eq)

instance Pos Campus where
  pos Kalvskinnet = (63.429, 10.388)
  pos Gløshaugen  = (63.416, 10.403)
  pos Tyholt      = (63.423, 10.435)
  pos Moholt      = (63.413, 10.434)
  pos Dragvoll    = (63.409, 10.471)
--class (Pos a) => Move a where
