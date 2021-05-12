{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import           RIO (Bool, HasLogFunc (..), LogFunc, lens)

-- | Command line arguments
newtype Options = Options
  { optionsVerbose :: Bool -- ^ Whether the logs should include debug messages.
  }

-- | The environment object that can be retrieved from inside the application.
data App = App
  { appLogFunc :: LogFunc -- ^ To be able to use `logInfo` and others.
  , appOptions :: Options -- ^ Command-line options to the application.
  }

data ServerState = ServerState

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
