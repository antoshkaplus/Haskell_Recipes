module GreatGood.FileStream where

import Control.Monad
import Data.Char


capitalize = forever $ do
    putStr "Give me some input: "
    line <- getLine
    putStrLn $ map toUpper line


capContent = do
    contents <- getContents
    putStr (map toUpper contents)


shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in result