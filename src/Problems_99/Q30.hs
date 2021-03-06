module Problems_99.Q30 where

import System.Random
import Control.Monad (replicateM)
import Data.List (nub, inits, tails, sortBy, groupBy)
import Data.Ord (comparing)

import qualified Problems_99.Q20 as Q20

-- 1 Problem 21
-- Insert an element at a given position into a list.
--
-- insertAt 'X' "abcd" 2
-- "aXbcd"

input_21 :: (Char, String, Int)
input_21 = ('X', "abcd", 2)

insertAt x xs i = let (x_0, x_1) = Q20.split xs (i-1) in x_0 ++ [x] ++ x_1

-- Q20.uncurry3 insertAt input_21


-- 2 Problem 22
-- Create a list containing all integers within a given range.
--
-- range 4 9
-- (4 5 6 7 8 9)

input_22 :: (Int, Int)
input_22 = (4, 9)

range :: Int -> Int -> [Int]
range i k = [i..k]

-- uncurry range input_22


-- 3 Problem 23
-- Extract a given number of randomly selected elements from a list.
-- (a b c d e f g h) 3
-- (e b a)


rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []
rnd_select l  n
    | n<0 = error "N must be greater than zero."
    | otherwise = do pos <- replicateM n $
                               getStdRandom $ randomR (0, (length l)-1)
                     return [l!!p | p <- pos]


-- randomRs
-- Plural variant of randomR, producing an infinite list of random values instead of returning a new generator.

--    rnd_select xs n = do
--        gen <- getStdGen
--        return $ take n [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]



--
--
-- 4 Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.
--
-- Example:
--
-- * (rnd-select 6 49)
-- (23 1 17 33 21 37)

rnd_select_2 n bound = do
    gen <- getStdGen
    return . take n . nub $ randomRs (1, bound) gen


--
-- 5 Problem 25
-- Generate a random permutation of the elements of a list.
--
-- Example:
--
-- * (rnd-permu '(a b c d e f))
-- (B A D C E F)
--

-- these one doesn't provide distinct, please change
rnd_permu :: [a] -> IO [a]
rnd_permu xs = rnd_select xs (length xs)

-- randomRIO - uses global random generator


--
--
-- 6 Problem 26
-- (**) Generate the combinations of K distinct objects chosen from the N elements of a list
--
-- In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
--
-- Example:
--
-- * (combinations 3 '(a b c d e f))
-- ((A B C) (A B D) (A B E) ... )
--

comb :: [a] -> [a] -> Int -> Int -> [[a]]
comb taken_items left_items left_count need_count
    | need_count == 0 = [taken_items]
    | left_count < need_count = []
    | otherwise = (comb ((head left_items):taken_items) (tail left_items) (left_count-1) (need_count-1)) ++
                  (comb taken_items (tail left_items) (left_count-1) need_count)

combinations n xs = comb [] xs (length xs) n

--    combinations :: Int -> [a] -> [[a]]
--    combinations 0 _  = [ [] ]
-- tails returns empty list too
--    combinations n xs = [ y:ys | y:xs' <- tails xs
--                               , ys <- combinations (n-1) xs']

combinations' :: Int -> [a] -> [([a],[a])]
combinations' 0 xs  = [ ([], xs) ]
combinations' n xs = [ (y:ys, x_other ++ y_other) | (x_other, y:x_next) <- zip (inits xs) (tails xs)
                                                  , (ys, y_other) <- combinations' (n-1) x_next]


--
-- 7 Problem 27
-- Group the elements of a set into disjoint subsets.
--
-- a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
--
-- Example:
--
-- * (group3 '(aldo beat carla david evi flip gary hugo ida))
-- ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) )
-- ... )
--
-- b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
--
-- Example:
--
-- * (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
-- ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) )
-- ... )
-- Note that we do not want permutations of the group members; i.e. ((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO BEAT) ...).
--
-- You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
--
-- Example in Haskell:
--
-- P27> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
-- (altogether 1260 solutions)
--
-- 27> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
-- (altogether 756 solutions)
--

group :: [Int] -> [a] -> [[[a]]]
group [n] elems = [[elems]]
group (n:ns) elems = [ pick:other_pick | (pick, other) <- combinations' n elems
                                       , other_pick <- group ns other]

--
-- 8 Problem 28
-- Sorting a list of lists according to length of sublists
--
-- a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length.
-- E.g. short lists first, longer lists later, or vice versa.
--
-- Example:
--
-- * (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
-- ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L))
-- Example in Haskell:
--
-- Prelude>lsort ["abc","de","fgh","de","ijkl","mn","o"]
-- Prelude>["o","de","de","mn","abc","fgh","ijkl"]
--
-- b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list
-- according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first,
-- others with a more frequent length come later.
--
-- Example:
--
-- * (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
-- ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))

lsort :: [[a]] -> [[a]]
lsort = sortBy $ comparing length

lfsort :: [[a]] -> [[a]]
lfsort xs = concat . lsort . groupBy (\x y -> (length x) == (length y)) $ lsort xs


-- Problem 29 and 30 don't exist