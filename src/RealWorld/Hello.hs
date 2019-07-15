module RealWorld.Hello where

import Data.List
import Data.Char
import Data.Text.Format
import Data.Either

niceDrop n xs | n <= 0 = xs
niceDrop _ [] = []
niceDrop n (_:xs) = niceDrop (n - 1) xs

-- implementation of foldl using foldr

foldR f z (x:xs) = f x $ foldR f z xs
foldR f z []   = z

foldL f z (x:xs) = foldL f (f z x) xs
foldL f z []   = z

-- Exercises p.97

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs

asInt :: String -> Int
asInt xs = loop 0 xs

asInt' :: String -> Either ErrorMessage Integer
asInt' = (foldr agg (Right 0)) . reverse where
    agg y (Left val) = Left val
    agg y (Right val)
        | isDigit y = Right $ val*10 + (toInteger . digitToInt) y
        | otherwise = Left $ "non-digit " ++ show y

asInt'' :: String -> Either ErrorMessage Integer
asInt'' ys@(x:xs)
    | x == '-' = (negate . asInt') xs
    | otherwise = asInt' ys
    where
        negate (Left val) = Left val
        negate (Right val) = Right (-1 * val)

type ErrorMessage = String

