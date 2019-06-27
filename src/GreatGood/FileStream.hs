module GreatGood.FileStream where

import Control.Monad
import Data.Char
import System.IO
import System.Directory
import Data.Typeable
import Data.List


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


interactShortLines = interact $ unlines . filter ((<10) . length) . lines


respondPalindromes contents = unlines (map (
    \xs -> if isPalindrome xs
           then "palindrome"
           else "not a palindrome")
    (lines contents))
    where isPalindrome xs = xs == reverse xs

respondPalindromes' = unlines .
    map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
    where isPalindrome xs = xs == reverse xs

readGirlfriend = do
    handle <- openFile "data/girlfriend.txt" ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

readGirlfriend' = do
    withFile "data/girlfriend.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        putStr contents)

removeTodoItem = do
    handle <- openFile "data/todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle

    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++
            line) [0..] todoTasks

    putStrLn "These are your TO-DO items:"
    putStr $ unlines numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine

    let number = read numberString
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"