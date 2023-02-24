{-# LANGUAGE FlexibleContexts, QuantifiedConstraints #-}

module Main where

import Prelude hiding (readFile)

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ask, MonadReader)
import Control.Monad.Trans.Reader (runReaderT)
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
        Just n -> 
            -- this is where the context gets passed to the top-level function
            let context = Context { n = n, name = name }
            in putStrLn =<< runReaderT msg context
    
-- record type for the context
data Context = Context
    { n :: Int
    , name :: String
    }

-- helper function
date :: IO (Integer, Int, Int) -- (year, month, day)
date = toGregorian . utctDay <$> getCurrentTime

-- create the first line of the poem from the name and the current year
intro :: (MonadIO m, MonadReader Context m) => m String
intro = do
    context <- ask
    (year, _, _) <- liftIO date
    pure $ "This poem is for their majesty " <> name context <> " in the year of our lord " <> show year

-- return the correct number of lines of the poem from a file
poem :: (MonadIO m, MonadReader Context m) => m [String]
poem = do
    context <- ask
    allLines <- liftIO $ readFile "./resources/tamerlane.txt"
    pure $ take (n context) (lines allLines)

-- put all the pieces together into one string 
msg :: (MonadIO m, MonadReader Context m) => m String
msg = do
    -- notice how I don't call `ask` here. I don't have to explicitly
    -- pass the context because both `intro` and `poem` use the same
    -- context as `msg`. I only call `ask` when I need a value from it.
    intro <- intro
    poem <- poem
    pure $ unlines (intro : poem)
