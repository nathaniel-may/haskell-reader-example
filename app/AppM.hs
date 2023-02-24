{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module AppM where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Reader (ask, MonadReader)
import Control.Monad.Trans.Reader (ReaderT(..))
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import qualified System.IO.Strict as IO


-- record type for the context
data Context = Context
    { n :: Int
    , name :: String
    }

-- Our application monad. It's common to have a TestM that runs effects differently in our tests.
newtype AppM a = AppM (ReaderT Context IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context)

-- Collapse all the effects into a single IO value that can be used in `main`.
runAppM :: AppM a -> Context -> IO a
runAppM (AppM m) context = runReaderT m context

-- define the ability to read time as "Now"
class Now m where
    now :: m (Integer, Int, Int) -- (year, month, day)

-- implement the ability to read time for our application monad
instance Now AppM where
    now = liftIO $ toGregorian . utctDay <$> getCurrentTime

-- define the ability to read files as "readFile"
class ReadFile m where
    readFile :: m String

-- implement the ability to read files for our application monad
instance ReadFile AppM where
    readFile = liftIO $ IO.readFile "./resources/tamerlane.txt"
