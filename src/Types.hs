{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Language.LSP.Types         (Uri)
import           Language.Python.Common.AST (ModuleSpan)
import           RIO


{-| Command line arguments. -}
newtype Options = Options
  { optionsVerbose :: Bool -- ^ Whether the logs should include debug messages.
  }


{-| The environment object that can be retrieved from inside the application. -}
data App = App
  { appLogFunc :: LogFunc -- ^ To be able to use `logInfo` and others.
  , appOptions :: Options -- ^ Command-line options to the application.
  }


{-| The read-write state for the language server. -}
newtype ServerState = ServerState
  { getFileIndex :: Map Uri (Text, ModuleSpan)
  }


{-| Logging function. -}
instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
