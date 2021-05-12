{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers (handlers) where

import           Control.Monad.Trans.State.Lazy (StateT)
import           Language.LSP.Server
import           Language.LSP.Types
import           RIO
import           Types

handlers :: Handlers (StateT ServerState (LspT () (RIO App)))
handlers = mconcat
  [ notificationHandler SInitialized $ \_not -> do
      let params = ShowMessageParams MtInfo (fromString "Hello, language client!")
      lift $ sendNotification SWindowShowMessage params
  , notificationHandler STextDocumentDidOpen $ \_not -> do
      let params = ShowMessageParams MtInfo (fromString "I see you've opened a Python file")
      lift $ sendNotification SWindowShowMessage params
  , notificationHandler STextDocumentDidClose $ \_not -> do
      let params = ShowMessageParams MtInfo (fromString "I see you've closed a Python file")
      lift $ sendNotification SWindowShowMessage params
  ]
