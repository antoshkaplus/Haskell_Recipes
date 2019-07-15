module Problems_99.Q50 where

import Data.List
import Control.Monad
import Data.Function (fix)

--    Problem 46
--    (**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for logical equivalence) which succeed or fail according to the result
--    of their respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.
--
--    A logical expression in two variables can then be written as in the following example: and(or(A,B),nand(A,B)).
--
--    Now, write a predicate table/3 which prints the truth table of a given logical expression in two variables.
--
--    table (\a b -> (and' a (or' a b)))
--
--    True True True
--    True False True
--    False True False
--    False False False


--    table :: (Bool -> Bool -> Bool) -> IO ()
--    table f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b)
--                                    | a <- [True, False], b <- [True, False]]

--    mapM_ is useful for executing something only for its side effects. For example, printing a string to standard output doesn't return
--    anything useful - it returns (). If we have a list of three strings, we would end up accumulating a list[(), (), ()].
--    Building this list has a runtime cost, both in terms of speed and memory usage, so by using mapM_ we can skip this step entirely.

--    table :: (Bool -> Bool -> Bool) -> IO ()
--    table f = putStrLn $ concatMap (++ "\n" )
--              [show a ++ " " ++ show b ++ " " ++ show (f a b)
--              | a <- [True, False], b <- [True, False] ]


--    Problem 47
--    (*) Truth tables for logical expressions (2).
--
--    Continue problem P46 by defining and/2, or/2, etc as being operators. This allows to write the logical expression in the more natural way,
--    as in the example: A and (A or not B). Define operator precedence as usual; i.e. as in Java.
--
--    table2 (\a b -> a `and'` (a `or'` not b))
--    True True True
--    True False True
--    False True False
--    False False False

--    !!!defining operator precedence!!!
--    infixl 4 `or'`
--    infixl 6 `and'`

--    Problem 48
--    (**) Truth tables for logical expressions (3).
--
--    Generalize problem P47 in such a way that the logical expression may contain any number of logical variables.
--    Define table/2 in a way that table(List,Expr) prints the truth table for the expression Expr, which contains the logical variables enumerated in List.
--
--    tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
--    -- infixl 3 `equ'`
--    True  True  True  True
--    True  True  False True
--    True  False True  True
--    True  False False True
--    False True  True  True
--    False True  False True
--    False False True  True
--    False False False True
--
--    -- infixl 7 `equ'`
--    True  True  True  True
--    True  True  False True
--    True  False True  True
--    True  False False False
--    False True  True  False
--    False True  False False
--    False False True  False
--    False False False False

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = putStrLn $ concatMap (++ "\n" )
          [concatMap (\x -> show x ++ " ") args ++ show (f args)
          | args <- replicateM n [True, False]]

--    Problem 49
--    (**) Gray codes.
--
--    An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
--
--    n = 1: C(1) = ['0','1'].
--    n = 2: C(2) = ['00','01','11','10'].
--    n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
--    Find out the construction rules and write a predicate with the following specification:
--
--    % gray(N,C) :- C is the N-bit Gray code
--    Can you apply the method of "result caching" in order to make the predicate more efficient, when it is to be used repeatedly?
--
--    gray 3
--    ["000","001","011","010","110","111","101","100"]

gray :: (Int -> [String]) -> Int -> [String]
gray f 0 = [""]
gray f n = let xs = f (n-1) in map ('0':) xs ++ map ('1':) (reverse xs)

memorize :: (Int -> a) -> (Int -> a)
memorize f = (map f [0 ..] !!)

--    will work but limited only within new computations
--    gray' :: Int -> [String]
--    grayCache' = memoize gray'

grayCache :: Int -> [String]
grayCache = fix (memorize . gray)

--    Problem 50
--    (***) Huffman codes.
--
--    We suppose a set of symbols with their frequencies, given as a list of fr(S,F) terms. Example: [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)].
--    Our objective is to construct a list hc(S,C) terms, where C is the Huffman code word for the symbol S.
--    In our example, the result could be Hs = [hc(a,'0'), hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')] [hc(a,'01'),...etc.].
--    The task shall be performed by the predicate huffman/2 defined as follows:
--
--    % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency table Fs
--
--    huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
--    [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]