module Problems_99.Q20 where

-- isRight is not imported by default
import Data.Either
import qualified Problems_99.Q10 as Q10


-- 1 Problem 11
-- (*) Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates
-- it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

-- (a a a a b c c a a d e e e e)
-- ((4 A) B (2 C) (2 A) D (4 E))

input_11 = [1,1,1,1,2,3,3,1,1,4,5,5,5,5]

type Elem a = Either (Q10.Code a) a

listToElem :: [x] -> Elem x
listToElem x = let len = length x
                   e = head x
               in if len == 1 then Right e else Left (Q10.Code len e)

encode :: (Eq a) => [a] -> [Elem a]
encode = map listToElem . Q10.pack

-- main = print $ encode input_11


-- 2 Problem 12
-- (**) Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

decodeElem :: Elem x -> [x]
decodeElem (Left (Q10.Code n a)) = replicate n a
decodeElem (Right x) = [x]

decode :: [Elem x] -> [x]
decode [] = []
decode (x:xs) = decodeElem x ++ decode xs

-- main = print $ decode $ encode input_11


-- 3 Problem 13
-- (**) Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates,
-- as in problem 9, but only count them.
-- As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

-- (a a a a b c c a a d e e e e)
-- ((4 A) B (2 C) (2 A) D (4 E))

input_13 = "aaaabccaadeeee"

join :: (Eq x) => x -> (Elem x) -> [Elem x]
join x_0 (Right x_1) = if x_0 == x_1
    then [Left (Q10.Code 2 x_0)]
    else [(Right x_0), (Right x_1)]
join x_0 (Left (Q10.Code n x_1)) = if x_0 == x_1
    then [Left (Q10.Code (n+1) x_0)]
    else [(Right x_0), (Left (Q10.Code n x_1))]

encode13 :: (Eq x) => [x] -> [Elem x]
encode13 [] = []
encode13 [x] = [Right x]
encode13 (x:xs) = (join x first) ++ rest where
        (first:rest) = encode13 xs

-- main = print $ encode input_13



-- 4 Problem 14
-- (*) Duplicate the elements of a list.
-- (a b c c d)
-- (a a b b c c c c d d)

input_14 = "abccd"
dupli [] = []
dupli (x:xs) = x:x: dupli xs

-- dupli input_14


-- 5 Problem 15
-- (**) Replicate the elements of a list a given number of times.
-- (a b c) 3
-- (a a a b b b c c c)

input_15 :: (String, Int)
input_15 = ("abc", 3)
repli :: [x] -> Int -> [x]
repli [] n = []
repli (x:xs) n = (replicate n x) ++ (repli xs n)

repli' [] n   = []
repli' (x:xs) n = (rep x n) ++ (repli' xs n)
    where rep x 0 = []
          rep x n = x:(rep x (n-1))

-- using monad
--  repli :: [a] -> Int -> [a]
--  repli xs n = xs >>= replicate n


-- uncurry repli input_15

-- 6 Problem 16
-- (**) Drop every N'th element from a list.
-- (a b c d e f g h i k) 3
-- (a b d e g h k)

input_16 :: (String, Int)
input_16 = ("abcdefghik", 3)
-- "abdeghk"

drop16 :: [x] -> Int -> [x]
drop16 xs n = [x | (x, i) <- zip xs [1..] , mod i n /= 0]

-- uncurry drop16 input_16


-- 7 Problem 17
-- (*) Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
-- (a b c d e f g h i k) 3
-- ((a b c) (d e f g h i k))

input_17 :: (String, Int)
input_17 = ("abcdefghik", 3)

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
-- pattern match 0 would be nice instead of condition
split l@(x : xs) n | n > 0     = (x : ys, zs)
                   | otherwise = ([], l)
    where (ys,zs) = split xs (n - 1)

--  other versions:
--  * split xs n = (take n xs, drop n xs)
--  * split = flip splitAt

-- uncurry split input_17


-- 8 Problem 18
-- (**) Extract a slice from a list.
-- Given two indices, i and k, the slice is the list
-- containing the elements between the i'th and k'th element
-- of the original list (both limits included). Start counting the elements with 1.

-- (a b c d e f g h i k) 3 7
-- (c d e f g)

input_18 :: (String, Int, Int)
input_18 = ("abcdefghik", 3, 7)

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs i k = [x | (x, j) <- zip xs [1..], j >= i && j <= k]

slice' xs i k = take (k-i+1) $ drop (i-1) xs


uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c)       = f a b c

-- uncurry slice input_18



-- 9 Problem 19
-- (**) Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
-- (a b c d e f g h) 3
-- (d e f g h a b c)

-- (a b c d e f g h) -2
-- (g h a b c d e f)

input_19_1 :: (String, Int)
input_19_2 :: (String, Int)

input_19_1 = ("abcdefgh", 3) -- "defghabc"
input_19_2 = ("abcdefgh", -2) -- "ghabcdef"

rotate :: [a] -> Int -> [a]
rotate l@(x:xs) n
    | n > 0 = rotate (xs ++ [x]) (n-1)
    | n == 0 = l
    | otherwise = rotate l (length l + n)

rotate' xs n
    | n == 0 = xs
    | n < 0  = rotate' xs $ (length xs) + n
    | otherwise =
        let (xs_1, xs_2) = splitAt n xs
        in xs_2 ++ xs_1


-- uncurry rotate input_19_1



-- 10 Problem 20
-- (*) Remove the K'th element from a list.

--(a b c d) 2
--(a c d)

input_20 = ("abcd", 2) -- "acd"

drop20 xs k = [x | (x, i) <- zip xs [1..], i /= k]

drop' xs k = let (xs_1, xs_2) = splitAt k xs
             in (init xs_1) ++ xs_2

-- uncurry drop input_20
