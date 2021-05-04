{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers (handlers) where

import           Language.LSP.Server
import           Language.LSP.Types
import           RIO

handlers :: Handlers (LspM ())
handlers = mconcat [
    notificationHandler SInitialized $ \_not -> do
      let params = ShowMessageParams MtInfo (fromString "Hello, language client!")
      sendNotification SWindowShowMessage params
  ]
