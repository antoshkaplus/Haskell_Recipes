module GreatGood.Randomness where

import System.Random

-- random :: (RandomGen g, Random a) => g -> (a, g)
-- mkStdGen :: Int -> StdGen

-- Random typeclass

randInt = random $ mkStdGen 100 :: (Int, StdGen)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (c, g) = random gen
        (c_2, g_2) = random g
        (c_3, g_3) = random g_2
    in (c, c_2, c_3)

-- randoms - returns infinite sequence of random numbers

finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n-1) newGen
    in (value:restOfList, finalGen)


