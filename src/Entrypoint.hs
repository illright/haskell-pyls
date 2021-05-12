{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Entrypoint (run) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Lazy (StateT (..), evalStateT)
import           Data.Map                       (empty)
import           Data.Version                   (showVersion)
import           Handlers                       (handlers)
import           Language.LSP.Server
import           Language.LSP.Types             (ServerInfo (ServerInfo),
                                                 TextDocumentSyncKind (..),
                                                 TextDocumentSyncOptions (..))
import qualified Paths_haskell_pyls
import           RIO
import           Types                          (App, ServerState (..))


serverName :: String
serverName = "haskell-pyls"

serverVersion :: String
serverVersion = showVersion Paths_haskell_pyls.version

thisServerInfo :: ServerInfo
thisServerInfo = ServerInfo (fromString serverName) (Just $ fromString serverVersion)

-- | Convert from 'IO a' to our preferred monad.
fromIOtoStateLspRIO :: IO a -> StateT ServerState (LspT () (RIO App)) a
fromIOtoStateLspRIO = liftIO

-- | Convert from our preferred monad to 'IO a'.
fromStateLspRIOtoIO
  :: App
  -> LanguageContextEnv ()
  -> ServerState
  -> StateT ServerState (LspT () (RIO App)) a
  -> IO a
fromStateLspRIOtoIO app env initialState =
  runRIO app . runLspT env . (`evalStateT` initialState)

run :: RIO App Int
run = do
  app <- ask
  let initialState = ServerState empty
  liftIO $ runServer $ ServerDefinition
    { defaultConfig = ()
    , onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \env _req -> pure $ Right env
    , staticHandlers = handlers
    , interpretHandler = \env ->
        Iso (fromStateLspRIOtoIO app env initialState) fromIOtoStateLspRIO
    , options = defaultOptions
      { serverInfo = Just thisServerInfo
      , textDocumentSync = Just TextDocumentSyncOptions
        { _openClose = Just True
        , _change = Just TdSyncFull
        , _willSave = Nothing
        , _willSaveWaitUntil = Nothing
        , _save = Nothing}
      }
    }
