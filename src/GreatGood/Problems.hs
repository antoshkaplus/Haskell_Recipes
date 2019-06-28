module GreatGood.Problems where

-- reverse Polish notation
solveRPN :: (Num a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*" = (x * y):ys
          foldingFunction (x:y:ys) "+" = (x + y):ys
          foldingFunction (x:y:ys) "-" = (y - x):ys
          foldingFunction xs numberString = read numberString:xs


toRPN :: String -> String
toRPN = unwords . words


minDistance :: (Num t, Ord t) => [t] -> (t, t)
minDistance [] = (0, 0)
minDistance (a:b:c:xs) = (min (a + aDist) (a + c + bDist), min (b + c + aDist) (b + bDist))
    where (aDist, bDist) = minDistance xs


data Label = A | B | C deriving (Show)
type Path = (Int, [Label])

add :: (Int, Label) -> Path -> Path
add (db, b) (da, as) = (da + db, b:as)

minPath :: Path -> Path -> Path
minPath pa@(a, _) pb@(b, _) = if a < b then pa else pb

-- returns A path and B path
minDistance' :: [Int] -> (Path, Path)
minDistance' [] = let emptyPath = (0, []) in (emptyPath, emptyPath)
minDistance' (a:b:c:xs) =
    let ((aDist, aLabels), (bDist, bLabels)) = minDistance' xs
    in (add (a, A) $ minPath (aDist, aLabels) (bDist + c, C:bLabels),
        add (b, B) $ minPath (aDist + c, C:aLabels) (bDist, bLabels))