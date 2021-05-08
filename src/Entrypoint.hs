{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Entrypoint (run) where

import           Control.Monad.IO.Class
import           Data.Version           (showVersion)
import           Handlers               (handlers)
import           Language.LSP.Server
import           Language.LSP.Types (ServerInfo (ServerInfo),
                                     TextDocumentSyncOptions(..))
import qualified Paths_haskell_pyls
import           RIO
import           Types                  (App)


serverName :: String
serverName = "haskell-pyls"

serverVersion :: String
serverVersion = showVersion Paths_haskell_pyls.version

thisServerInfo :: ServerInfo
thisServerInfo = ServerInfo (fromString serverName) (Just $ fromString serverVersion)

run :: RIO App Int
run = do
  liftIO $ runServer $ ServerDefinition
    { defaultConfig = ()
    , onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = defaultOptions 
      { serverInfo = Just thisServerInfo
      , textDocumentSync = Just TextDocumentSyncOptions
        { _openClose = Just True
        , _change = Nothing
        , _willSave = Nothing
        , _willSaveWaitUntil = Nothing
        , _save = Nothing}
      }
    }
