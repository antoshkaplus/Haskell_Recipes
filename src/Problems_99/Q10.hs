module Problems_99.Q10 where

import Data.List (group)


short_list = [1,2,3,4]


-- 1 Problem 1
-- (*) Find the last element of a list.
-- Prelude> myLast [1,2,3,4]
-- 4

input_1 = short_list
sol_1 = last

sol_1' [x] = x
sol_1' (x:xs) = sol_1' xs

-- 2 Problem 2
-- (*) Find the last but one element of a list.
-- Prelude> myButLast [1,2,3,4]
-- 3

input_2 = short_list
sol_2 = last . init

sol_2' [x,y] = x
sol_2' (x:xs) = sol_2' xs

-- 3 Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.
-- * (element-at '(a b c d e) 3)
-- c

input_3 = (short_list, 1)
sol_3 list = (list !!)

sol_3' (x:xs) 1 = x
sol_3' (x:xs) k = sol_3' xs (k-1)


-- USING LET SYNTAX
-- main = print $ let (list, index) = input_3 in sol_3 list index


-- 4 Problem 4
-- (*) Find the number of elements of a list.
-- Prelude> myLength [123, 456, 789]
-- 3

input_4 = short_list
-- need to define function as there are multiple different
-- length functions
sol_4 :: [a] -> Int
sol_4 = length

sol_4' [] = 0
sol_4' (x:xs) = 1 + sol_4' xs


-- 5 Problem 5
-- (*) Reverse a list.
-- Prelude> myReverse "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"

input_5 = short_list
sol_5 = reverse

sol_5' ps = let rev (x:xs) ys = rev xs (x:ys)
                rev [] ys = ys
            in rev ps []


-- 6 Problem 6
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
-- Q10> isPalindrome [1,2,3]
-- False
-- Q10> isPalindrome "madamimadam"
-- True
-- Q10> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs = (head xs) == (last xs) && (isPalindrome $ init $ tail xs)

input_6_1 = short_list
input_6_2 = short_list ++ reverse short_list


palindrome :: (Eq a) => [a] -> Bool
palindrome xs = p [] xs xs
   where p rev (x:xs) (_:_:ys) = p (x:rev) xs ys
         p rev (x:xs) [_] = rev == xs
         p rev xs [] = rev == xs

-- main = print $ isPalindrome input_6_2


-- 7 Problem 7
-- (**) Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a 'flat' list
-- by replacing each list with its elements (recursively).
-- flatten (a (b (c d) e))
-- (a b c d e)

-- data type with value constructors
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
-- from flatten we always usually return some sort of list
-- calling concatMap would concatenate them
-- thats why we should be fine here
flatten (List x) = concatMap flatten x


flatten' (Elem x) = [x]
flatten' (List []) = []
flatten' (List (x:xs)) = (flatten' x) ++ (flatten' (List xs))


input_7 = List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]
sol_7 = flatten

-- main = print $ flatten input_7


-- 8 Problem 8
-- (**) Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single copy of the element.
-- The order of the elements should not be changed.
-- compress (a a a a b c c a a d e e e e)
-- (a b c a d e)

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (x:xs) = (if x == head xs then [] else [x]) ++ compress xs

compress' (x:ys@(y:_))
    | x == y    = compress' ys
    | otherwise = x : compress' ys
compress' ys = ys

-- group - splits its list argument into a list of lists of equal, adjacent elements
-- Eq a => [a] -> [[a]]
compress'' :: Eq a => [a] -> [a]
compress'' = map head . group

-- main = print $ compress "aaaabccaadeeee"



-- 9 Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.
-- pack (a a a a b c c a a d e e e e)
-- ((a a a a) (b) (c c) (a a) (d) (e e e e))

-- functional program can be implemented in different ways and understand which one is
-- correct is very hard

-- here we compose from going backward

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [a] = [[a]]
pack (x:xs)
    | x == head first = (x : first) : rest
    | otherwise       = [x] : first : rest
    where (first:rest) = pack xs

pack' x = group x

pack'' ts = let
    p [] ys = ys
    p (x:xs) [] = p xs [[x]]
    p (x:xs) (y:ys)
        | x == head y = p xs ((x:y):ys)
        | otherwise   = p xs ([x]:y:ys)
    in reverse $ p ts []

pack''' [] = []
pack''' xs = let (xs_1, xs_2) = span (==(head xs)) xs
             in xs_1 : (pack xs_2)


input_9 = [1, 1, 1, 1, 2, 3, 3, 1, 1, 4, 5, 5, 5, 5]
-- main = print $ pack input_9


-- AWESOME SOLUTION
-- pack (x:xs) = let (first,rest) = span (==x) xs
--                in (x:first) : pack rest
-- pack [] = []
--
-- -- This is implemented as group in Data.List.
-- -- A more verbose solution is
--
-- pack :: Eq a => [a] -> [[a]]
-- pack [] = []
-- pack (x:xs) = (x:first) : pack rest
--          where
--            getReps [] = ([], [])
--            getReps (y:ys)
--                    | y == x = let (f,r) = getReps ys in (y:f, r)
--                    | otherwise = ([], (y:ys))
--            (first,rest) = getReps xs


-- 10 Problem 10
-- (*) Run-length encoding of a list. Use the result of problem P09 to implement
-- the so-called run-length encoding data compression method.
-- Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

-- encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

input_10 = "aaaabccaadeeee"

data Code a = Code Int a deriving (Show)

encode :: (Eq a) => [a] -> [Code a]
-- map something return ([[a]] -> [Code a]) and pack ([a] -> [[a]]) => [a] -> [Code a]
-- somehow it works
encode = map (\x -> Code (length x) (head x)) . pack

encode' xs = [(length x, head x) | x <- group xs]

-- main = print $ encode input_10