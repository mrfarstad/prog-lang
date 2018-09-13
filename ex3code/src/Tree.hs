module Tree
  ( Tree(..)
  ) where

-- TASK 3.2
-- Binary Trees
data Tree a
  = Branch (Tree a)
           a
           (Tree a)
  | Leaf a
  deriving (Eq, Show)

-- The Foldable instance might prove tricky to define, so
-- defining the specific functions first may be easier!
treeSum :: (Num a) => Tree a -> a
treeSum (Leaf a)         = a
-- Branch :: Tree a -> a -> Tree a -> Tree a
treeSum (Branch t1 a t2) = a + treeSum t1 + treeSum t2

treeConcat :: Tree String -> String
treeConcat (Leaf a)         = a
treeConcat (Branch t1 a t2) = a ++ treeConcat t1 ++ treeConcat t2

treeMaximum :: (Ord a) => Tree a -> a
treeMaximum (Leaf a) = a
treeMaximum (Branch t1 a t2)
  | a >= t1max = a
  | a < t1max = t1max
  | a >= t2max = a
  | a < t2max = t2max
  where
    t1max = treeMaximum t1
    t2max = treeMaximum t2

-- Write a Foldable instance for Tree.
instance Foldable Tree
-- foldr :: (a -> b -> b) -> b -> t a -> b
                                           where
  foldr op acc (Leaf a)         = op a acc
  foldr op acc (Branch t1 a t2) = foldr op (op a $ foldr op acc (t1)) t2
