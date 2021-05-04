{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers (handlers) where

import           Data.String
import           Language.LSP.Server
import           Language.LSP.Types
import           Prelude
-- import           RIO

handlers :: Handlers (LspM ())
handlers = mconcat [
    notificationHandler SInitialized $ \_not -> do
      let params = ShowMessageParams MtInfo (fromString "Hello, language client!")
      sendNotification SWindowShowMessage params
  ]
