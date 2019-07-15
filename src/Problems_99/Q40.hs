module Problems_99.Q40 where

import Data.List

--    Problem 31
--    (**) Determine whether a given integer number is prime.
--
--    isPrime 7
--    True
--

isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | otherwise = not . any (\x -> (mod n x) == 0) $ takeWhile (<n) [2..]

--
--    Problem 32
--    (**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.
--
--    [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
--    [9,3,3]
--

myGCD :: Integer -> Integer -> Integer
myGCD a b
      | b == 0     = abs a
      | otherwise  = myGCD b (a `mod` b)

--    Problem 33
--    (*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
--
--    coprime 35 64
--    True
--

coprime x y = (==) 1 $ myGCD x y

--    Problem 34
--    (**) Calculate Euler's totient function phi(m).
--
--    Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r < m) that are coprime to m.
--
--    Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
--
--    totient 10
--    4
--

totient n = length [k | k <- [1..n-1], coprime n k]

--    Problem 35
--    (**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.
--
--    primeFactors 315
--    [3, 3, 5, 7]
--

primeFactors :: Int -> [Int]
primeFactors n = let p = head [k | k <- [2..n], mod n k == 0]
                 in if p == n then [p] else primeFactors p ++ primeFactors (quot n p)

--factor :: Integer -> [Integer]
--
--factor 1 = []
--factor n = let prime = head $ dropWhile ((/= 0) . mod n) [2 .. n]
--           in (prime :) $ factor $ div n prime


--    Problem 36
--    (**) Determine the prime factors of a given positive integer.
--    Construct a list containing the prime factors and their multiplicity.
--
--    prime_factors_mult 315
--    [(3,2),(5,1),(7,1)]

prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult = (map (\x -> (head x, length x))) . group . primeFactors

--    Problem 37
--    (**) Calculate Euler's totient function phi(m) (improved).
--
--    See problem 34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem 36
--    then the function phi(m) can be efficiently calculated as follows: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities)
--    of a given number m. Then phi(m) can be calculated with the following formula:
--
--    phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
--             (p2 - 1) * p2 ** (m2 - 1) *
--             (p3 - 1) * p3 ** (m3 - 1) * ...
--    Note that a ** b stands for the b'th power of a.

phi :: [(Integer, Integer)] -> Integer
phi xs = foldr1 (*) $ map (\(p, m) -> (p-1)*p^(m-1)) xs

--    totient m = product [(p - 1) * p ^ (c - 1) | (p, c) <- prime_factors_mult m]


--    Problem 38
--    (*) Compare the two methods of calculating Euler's totient function.
--    Use the solutions of problems 34 and 37 to compare the algorithms. Take the number of reductions as a measure for efficiency. Try to calculate phi(10090) as an example.
--
--    (no solution required)

-- the second solution is better, since immediatly reduces number of iterations
-- the whole thing is around linear time, while the first algo has burden of looking for GCD each time
-- which is linear operation by itself (maybe logarithmic though).


--    Problem 39
--    (*) A list of prime numbers.
--    Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
--
--    primesR 10 20
--    [11,13,17,19]

--    slow, beatiful but not my code
primesR :: Integral a => a -> a -> [a]
primesR a b = takeWhile (<= b) $ dropWhile (< a) $ sieve [2..]
  where sieve (n:ns) = n:sieve [ m | m <- ns, m `mod` n /= 0 ]


--    Problem 40
--    (**) Goldbach's conjecture.
--
--    Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. Example: 28 = 5 + 23.
--    It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically
--    confirmed up to very large numbers (much larger than we can go with our Prolog system).
--    Write a predicate to find the two prime numbers that sum up to a given even integer.
--
--    goldbach 28
--    (5, 23)

goldbach n = let p = head [k | k <- [2..], isPrime k, isPrime (n-k)] in (p, n-p)

--    goldbach n = head [(x,y) | x <- primesR 2 (n-2),
--                               let y = n-x, isPrime y]


--    Problem 41
--    (**) Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
--
--    In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than say 50.
--    Try to find out how many such cases there are in the range 2..3000.
--
--    Example:
--
--    * (goldbach-list 9 20)
--    10 = 3 + 7
--    12 = 5 + 7
--    14 = 3 + 11
--    16 = 3 + 13
--    18 = 5 + 13
--    20 = 3 + 17
--    * (goldbach-list 1 2000 50)
--    992 = 73 + 919
--    1382 = 61 + 1321
--    1856 = 67 + 1789
--    1928 = 61 + 1867
--    Example in Haskell:
--
--    goldbachList 9 20
--    [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
--    goldbachList' 4 2000 50
--    [(73,919),(61,1321),(67,1789),(61,1867)]

goldbachList n_1 n_2 = map goldbach [k | k <- [n_1..n_2], even k]

--    goldbachList lb ub = map goldbach $ [even_lb,even_lb+2..ub]
--        where even_lb = max ((lb+1) `div` 2 * 2) 4
--    goldbachList' lb ub mv = filter (\(a,b) -> a > mv && b > mv) $
--                             goldbachList lb ub

--    goldbachList n m = map goldbach $ dropWhile (<4) $ filter even [n..m]
--    goldbachList' n m i = filter (\(x,y) -> x > i && y > i) $ goldbachList n m