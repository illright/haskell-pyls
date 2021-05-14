{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers (handlers) where

import           Control.Monad.Trans.State.Lazy  (StateT, get, put)
import           Data.Map
import qualified Data.Text                       as T
import           Language.LSP.Server
import           Language.LSP.Types
import qualified Language.Python.Common          as PyCommon
import qualified Language.Python.Common.AST      as PyAST
import qualified Language.Python.Version3.Parser as PyV3
import           RIO
import           RIO.Text
import           System.IO
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

      -- lift $ lift $ logInfo (fromString $ show currentFileIndex)
      let newFileIndex = alter (replaceFileContent contentChanges fileName) uri currentFileIndex
      put (ServerState newFileIndex)
      -- lift $ lift $ logInfo (fromString $ show newFileIndex)
  , requestHandler STextDocumentFormatting $ \req responder -> do
      let RequestMessage _jsonrpc _id _method params = req
      let DocumentFormattingParams _workDoneToken textDocument _formattingOptions = params
      let TextDocumentIdentifier uri = textDocument
      case uriToFilePath uri of
        Nothing -> responder (Left $ ResponseError UnknownErrorCode (fromString "error") Nothing)
        Just filePath -> do
          pythonFile <- liftIO $ openFile filePath ReadMode
          contents <- liftIO $ hGetContents pythonFile
          let prettifiedContent = prettifyFileContent (fromString contents)

          let fullDocumentRange = Range (Position 0 0) (getLastPosition (fromString contents))
          responder (Right $ List [TextEdit fullDocumentRange prettifiedContent])
  ]

getLastPosition
  :: Text
  -> Position
getLastPosition fileContent = Position lineNum columnNum
  where
    splitContent = RIO.Text.lines fileContent
    lineNum = max (RIO.length splitContent - 1) 0
    columnNum = lastElementLength splitContent

    lastElementLength :: [Text] -> Int
    lastElementLength [] = 0
    lastElementLength [word] = RIO.Text.length word
    lastElementLength (_x:xs) = lastElementLength xs


prettifyFileContent
  :: Text
  -> Text
prettifyFileContent fileContent = prettifierResult
  where
    parseResult = PyV3.parseModule (T.unpack fileContent) "untitled.py"
    prettifierResult = case parseResult of
      Left _e                   -> fileContent
      Right (newAST, _comments) -> fromString (PyCommon.prettyText newAST)


replaceFileContent
  :: List TextDocumentContentChangeEvent
  -> String
  -> Maybe (Text, PyAST.ModuleSpan)
  -> Maybe (Text, PyAST.ModuleSpan)
replaceFileContent (List []) _fileName _currentFile = Nothing
replaceFileContent (List (change : _restChanges)) fileName _currentFile =
    case parseResult of
      Left _e                   -> Nothing
      Right (newAST, _comments) -> Just (fileContent, newAST)
  where
    TextDocumentContentChangeEvent _range _rangeLength fileContent = change
    parseResult = PyV3.parseModule (T.unpack fileContent) fileName
