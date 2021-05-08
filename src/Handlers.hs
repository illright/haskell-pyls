{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers (handlers) where

import           Language.LSP.Server
import           Language.LSP.Types
import           RIO

handlers :: Handlers (LspM ())
handlers = mconcat 
  [ notificationHandler SInitialized $ \_not -> do
      let params = ShowMessageParams MtInfo (fromString "Hello, language client!")
      sendNotification SWindowShowMessage params
  , notificationHandler STextDocumentDidOpen $ \_not -> do
      let params = ShowMessageParams MtInfo (fromString "I see you've opened a Python file")
      sendNotification SWindowShowMessage params
  , notificationHandler STextDocumentDidClose $ \_not -> do
      let params = ShowMessageParams MtInfo (fromString "I see you've closed a Python file")
      sendNotification SWindowShowMessage params
  ]
