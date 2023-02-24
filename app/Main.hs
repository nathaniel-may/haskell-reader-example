module Main where

import Prelude hiding (readFile)

import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import System.IO.Strict (readFile)
import Text.Read (readMaybe)


main :: IO ()
main = do
    -- collect user info
    putStrLn "What is your name?"
    name <- getLine
    putStrLn "How many lines of poetry do you want?"
    n' <- getLine
    -- print an empty line
    putStrLn ""
    -- parse number
    case readMaybe n' of
        Nothing -> putStrLn $ show n' <> " is not an integer"
        -- print output
        Just n -> putStrLn =<< msg name n
    

-- helper function
date :: IO (Integer, Int, Int) -- (year, month, day)
date = toGregorian . utctDay <$> getCurrentTime

-- create the first line of the poem from the name and the current year
intro :: String -> IO String
intro name = do
    (year, _, _) <- date
    pure $ "This poem is for their majesty " <> name <> " in the year of our lord " <> show year

-- return the correct number of lines of the poem from a file
poem :: Int -> IO [String]
poem n = do
    allLines <- readFile "./resources/tamerlane.txt"
    pure $ take n (lines allLines)

-- put all the pieces together into one strin 
msg :: String -> Int -> IO String
msg name n = do
    intro <- intro name
    poem <- poem n
    pure $ unlines (intro : poem)
