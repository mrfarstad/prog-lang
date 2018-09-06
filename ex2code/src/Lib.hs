module Lib
  ( f0
  , f1
  , f2
  , take
  , map
  , iterate
  , filterPos
  , filterPosMany
  , flip3
  , Maybe(..)
  , safeHeadList
  , safeHead
  , isPerfSq
  , accuracy
  ) where

import           Prelude hiding (Maybe, iterate, map, sqrt, take)

-- TASK 1
-- Parametric polymorphism
-- Below are three type signatures. Can you implement them? We
-- say a function or implementation /inhabits/ it's type
-- (signature). How many other inhabitants do these types
-- have? What if we fixed a = Int, does that change your
-- previous answer?
f0 :: a -> a
f0 a = a

f1 :: a -> b -> a
f1 a b = a

f2 :: a -> b -> b
f2 a b = b

-- Rewrite the function "takeInt" from exercice 1 as "take" so
-- that it accepts a list of any type. If you used the
-- built-in function "take" on the last assignment, write your
-- own implementation this time. Be sure to include a type
-- signature. (Hint: If you already wrote takeInt, you won't
-- have to change much.)
take :: Int -> [a] -> [a]
take n li =
  if n > length li
    then li
    else [li !! x | x <- [0 .. n - 1]]

-- The function head :: [a] -> a which returns the first
-- element of a list, is /partial/, meaning it will crash for
-- some inputs. (Which?) One solution could be to make a
-- /total/ function "safeHeadList :: [a] -> [a]" which either
-- gives the head, or nothing. Can you implement it using take?
safeHeadList :: [a] -> [a]
safeHeadList = take 1

-- The output of safeHeadList is either empty or a singleton,
-- and thus using a list as output-type is a bit misleading. A
-- better choice is Maybe (sometimes called Optional):
data Maybe a
  = Some a
  | None
  deriving (Eq, Show)

-- Implement 'safeHead', representing failure using None.
safeHead :: [a] -> Maybe a
safeHead []     = None
safeHead (x:xs) = Some x

-- TASK 2
-- Higher order functions
map :: (a -> b) -> [a] -> [b]
map func arr = [func e | e <- arr]

iterate :: (a -> a) -> a -> [a]
iterate func element = element : iterate func (func element)

-- TASK 3
-- Currying and partial application
-- complete the function filterPos
-- that takes a list and returns
-- a filtered list containing only positive
-- integers (including zero)
-- use partial application to achieve this
filterPos :: [Int] -> [Int]
filterPos = filter (>= 0)

-- complete the function filterPosMany
-- that takes a list of lists and returns
-- a list of lists with only positive
-- integers (including zero)
-- hint: use filterPos and map
filterPosMany :: [[Int]] -> [[Int]]
filterPosMany l = map filterPos l

flip3 :: (a -> b -> c -> d) -> c -> b -> a -> d
flip3 func c b a = func a b c

-- TASK 4
-- Infinite lists
newton :: Double -> Double -> Double
newton x x0 = x0 - (x0 ^ 2 - x) / (2 * x0)

genApp :: Double -> [Double]
genApp x = iterate (newton x) x

isInteger :: Double -> Bool
isInteger x = x == fromInteger (round x)

approx :: Double -> [Double] -> Double
approx x (a:aa) =
  if a - head aa < x
    then head aa
    else approx x aa

isPerfSq :: Double -> Bool
isPerfSq x = isInteger (approx 0.000000001 (genApp x))

--uncomment when isPerfSqr is defined
accuracy :: Int -> Bool
accuracy x = take x generated == take x [x ^ 2 | x <- [1 ..]]
  where
    zpd = zip [1 ..] (map isPerfSq [1 ..])
    f (x, y) = y == True
    generated = fst . unzip $ filter f zpd
