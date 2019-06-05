module GreatGood.GreatGood where

-- head
-- tail
-- init
-- last
-- length
-- null - checks if list is empty
-- reverse
-- take - take first number of elements
-- drop - drop first number of elements
-- maximum
-- minimum
-- sum
-- product
-- elem - checks if element is inside or not

-- double dots
-- cycle
-- repeat
-- replicate

-- list comprehensions
-- fst, snd

-- Haskell is lazy
-- zip

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"


bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat = "You're fat! Lose some weight, fatty!"
    | otherwise = "You're a whale, congratulations!"
    -- have to be on the same level
    where bmi = weight / height^2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

cylinder' :: (RealFloat a) => a -> a -> a
cylinder' r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + 2 * topArea



head' :: [a] -> a
head' [] = error "No head for empty lists!"
-- can it work with all the tuples???
head' (x:_) = x

--head' :: [a] -> a
--head' xs = case xs of [] -> error "No head for empty lists!"


-- let binding to introduce function in the local scope

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

--calcBmis :: (RealFloat a) => [(a, a)] -> [a]
--calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

-- what xs is a function call
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs


compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100


-- partial application
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y


chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)

-- lambdas
numLongChains :: Int
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0


-- so we usually use right folds when we're building up new lists from a list
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- One big difference is that right folds
--   work on infinite lists, whereas left ones don't! To put it plainly, if you
--   take an infinite list at some point and you fold it up from the right,
--   you'll eventually reach the beginning of the list. However, if you take
--   an infinite list at a point and you try to fold it up from the left, you'll
--   never reach an end!


-- scanl and scanr are like foldl and foldr, only they report all the intermediate accumulator states
-- in the form of a list. There are also scanl1 and scanr1, which are analogous to foldl1 and foldr1.


-- $
-- map ($ 3) [(4+), (10*), (^2), sqrt]
-- [7.0,30.0,9.0,1.7320508075688772]


-- function composition
--fn x = ceiling (negate (tan (cos (max 50 x))))
fn = ceiling . negate . tan . cos . max 50

-- modules

-- So don't put type constraints into data declarations even if it seems to make sense, because you'll
-- have to put them into the function type declarations either way.

-- Once again, it's very important to distinguish between the type constructor and the value constructor


data Person = Person {

    firstName :: String,
    lastName :: String,
    age :: Int

} deriving (Eq)

-- type String = [Char]
-- can be parametrized
-- type AssocList k v = [(k,v)]


-- Prelude definition
--class Eq a where
--    (==) :: a -> a -> Bool
--    (/=) :: a -> a -> Bool
--    x == y = not (x /= y)
--    x /= y = not (x == y)

-- fmap
-- exceptions







