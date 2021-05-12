{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers (handlers) where

import           Control.Monad.Trans.State.Lazy  (StateT, get, put)
import           Data.Map
import qualified Data.Text                       as T
import           Language.LSP.Server
import           Language.LSP.Types
import qualified Language.Python.Common.AST      as P
import qualified Language.Python.Version3.Parser as V3
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
  , notificationHandler STextDocumentDidChange $ \changeNotification -> do
      ServerState currentFileIndex <- get

      let NotificationMessage _jsonrpc _method params = changeNotification
      let DidChangeTextDocumentParams textDocument contentChanges = params
      let VersionedTextDocumentIdentifier uri _version = textDocument
      let fileName = "untitled.py"

      lift $ lift $ logInfo (fromString $ show currentFileIndex)
      let newFileIndex = alter (replaceFileContent contentChanges fileName) uri currentFileIndex
      put (ServerState newFileIndex)
      lift $ lift $ logInfo (fromString $ show newFileIndex)
  ]

replaceFileContent
  :: List TextDocumentContentChangeEvent
  -> String
  -> Maybe (Text, P.ModuleSpan)
  -> Maybe (Text, P.ModuleSpan)
replaceFileContent (List []) _fileName _currentFile = Nothing
replaceFileContent (List (change : _restChanges)) fileName _currentFile =
    case parseResult of
      Left _e                   -> Nothing
      Right (newAST, _comments) -> Just (fileContent, newAST)
  where
    TextDocumentContentChangeEvent _range _rangeLength fileContent = change
    parseResult = V3.parseModule (T.unpack fileContent) fileName
