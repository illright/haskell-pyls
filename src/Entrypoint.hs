{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Entrypoint (run) where

import           Data.Text           (pack)
import           Data.Version        (showVersion)
import           Language.LSP.Server
import           Language.LSP.Types
import qualified Paths_haskell_pyls
import           RIO
import           Types               (App)

handlers :: Handlers (LspM ())
handlers = mempty

serverName :: String
serverName = "haskell-pyls"

serverVersion :: String
serverVersion = showVersion Paths_haskell_pyls.version

thisServerInfo :: ServerInfo
thisServerInfo = ServerInfo (pack serverName) (Just $ pack serverVersion)

run :: RIO App Int
run = do
  --logInfo ("Starting " ++ serverName ++ "(v" ++ serverVersion ++ ")")
  liftIO $ runServer $ ServerDefinition
    { defaultConfig = ()
    , onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers
    , interpretHandler = \env -> Iso (runLspT env) liftIO
    , options = defaultOptions { serverInfo = Just thisServerInfo }
    }
