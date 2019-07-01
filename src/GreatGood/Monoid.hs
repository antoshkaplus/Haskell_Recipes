module GreatGood.Monoid where


import Data.Monoid
import qualified Data.Foldable as F


lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend`
    (vowels x `compare` vowels y) `mappend`
    (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x `mappend`
                             F.foldMap f r

newtype TreeCenter a = TreeCenter { getTree :: Tree a }

instance F.Foldable TreeCenter where
    foldMap f (TreeCenter Empty) = mempty
    foldMap f (TreeCenter (Node x l r)) = f x `mappend`
                                          F.foldMap f l `mappend`
                                          F.foldMap f r

-- not an ordered binary tree
testTree = Node 5
    (Node 3
        (Node 1 Empty Empty)
        (Node 6 Empty Empty)
    )
    (Node 9
        (Node 8 Empty Empty)
        (Node 10 Empty Empty)
    )

-- F.foldl (\x y -> y : x) [] testTree
-- [10,9,8,5,6,3,1]

-- F.foldr (:) [] testTree
-- [1,3,6,5,8,9,10]


-- F.foldr (:) [] $ TreeCenter testTree
-- [5,1,3,6,8,9,10]

-- F.foldl (\x y -> y : x) [] $ TreeCenter testTree
-- [10,9,8,6,3,1,5]
