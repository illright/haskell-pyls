{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Entrypoint (run) where

import           Language.LSP.Server
import           RIO
import           Types               (App)

handlers :: Handlers (LspM ())
handlers = mempty

run :: RIO App Int
run = do
  logInfo "Starting the language server"
  liftIO $ runServer $ ServerDefinition
    { onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = defaultOptions
    }
