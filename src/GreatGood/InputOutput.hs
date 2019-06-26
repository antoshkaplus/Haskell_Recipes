module GreatGood.InputOutput where

import Data.Char

name :: IO()
name = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

fullName :: IO()
fullName = do
    putStrLn "What's your first name?"
    firstName <- getLine
    putStrLn "What's your last name?"
    lastName <- getLine
    let bigFirstName = map toUpper firstName
        bigLastName = map toUpper lastName
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"


-- return wraps something in IO object
-- return and <- are opposite
infiniteLineReader :: IO()
infiniteLineReader = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn $ reverseWords line
            infiniteLineReader

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- when Control.Monad
-- sequence

-- sequence (map print [1,2,3,4,5])

-- mapM takes a function and a list, maps the function over the list and then sequences it.
-- We usually use mapM_ when we don't care what result our sequenced I/O actions have.