{-# LANGUAGE FlexibleContexts #-}

module Main where

import Prelude hiding (readFile)

import AppM (Context(..), n, name, now, Now, readFile, ReadFile, runAppM)
import Control.Monad.Reader (ask, MonadReader)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Maybe (fromMaybe)
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
        Just n -> 
            -- this is where the context gets passed to the top-level function
            let context = Context { n = n, name = name }
            in putStrLn =<< runAppM msg context

-- create the first line of the poem from the name and the current year
intro :: (Now m, MonadReader Context m) => m String
intro = do
    context <- ask
    (year, _, _) <- now
    pure $ "This poem is for their majesty " <> name context <> " in the year of our lord " <> show year

-- return the correct number of lines of the poem from a file
poem :: (ReadFile m, MonadReader Context m) => m [String]
poem = do
    context <- ask
    allLines <- readFile
    pure $ take (n context) (lines allLines)

-- put all the pieces together into one string 
msg :: (Now m, ReadFile m, MonadReader Context m) => m String
msg = do
    -- notice how I don't call `ask` here. I don't have to explicitly
    -- pass the context because both `intro` and `poem` use the same
    -- context as `msg`. I only call `ask` when I need a value from it.
    intro <- intro
    poem <- poem
    pure $ unlines (intro : poem)
