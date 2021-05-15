{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import           Control.Monad.Trans.State.Lazy     (StateT, get, put)
import           Data.Map
import qualified Data.Text                          as T
import           Language.LSP.Server
import           Language.LSP.Types
import qualified Language.Python.Common             as PyCommon
import qualified Language.Python.Common.AST         as PyAST
import qualified Language.Python.Version3.Parser    as PyV3
import           RIO
import qualified RIO.List
import           RIO.Text
import           Symbols                            (getSymbols)
import           System.IO
import           Types

import           Control.Lens                       (toListOf)
import           Data.Data.Lens                     (template)
import qualified Language.Python.Common.SrcLocation as PythonSrcLoc
import           System.Directory                   (getCurrentDirectory)
import           System.Directory.Tree              (AnchoredDirTree (..),
                                                     DirTree (..), filterDir,
                                                     readDirectory)
import           System.FilePath                    (joinPath, takeExtension)


{-| List of handlers defined for our language server. -}
handlers :: Handlers (StateT ServerState (LspT () (RIO App)))
handlers = mconcat
  -- Handle file initialization
  [ notificationHandler SInitialized $ \_not -> do
      let params = ShowMessageParams MtInfo (fromString "Hello, language client!")
      lift $ sendNotification SWindowShowMessage params
  -- Handle file opening
  , notificationHandler STextDocumentDidOpen $ \_not -> do
      let params = ShowMessageParams MtInfo (fromString "I see you've opened a Python file")
      lift $ sendNotification SWindowShowMessage params
  -- Handle file closing
  , notificationHandler STextDocumentDidClose $ \_not -> do
      let params = ShowMessageParams MtInfo (fromString "I see you've closed a Python file")
      lift $ sendNotification SWindowShowMessage params
  -- Handle file changing (note: unfinished)
  , notificationHandler STextDocumentDidChange $ \changeNotification -> do
      ServerState currentFileIndex <- get

      let NotificationMessage _jsonrpc _method params = changeNotification
      let DidChangeTextDocumentParams textDocument contentChanges = params
      let VersionedTextDocumentIdentifier uri _version = textDocument
      let fileName = "untitled.py"

      let newFileIndex = alter (replaceFileContent contentChanges fileName) uri currentFileIndex
      put (ServerState newFileIndex)

      let messageParams = ShowMessageParams MtInfo (fromString "I see you've changed a Python file")
      lift $ sendNotification SWindowShowMessage messageParams
  -- Handle file formatting (note: formatter skips python comments)
  , requestHandler STextDocumentFormatting $ \req responder -> do
      let RequestMessage _jsonrpc _id _method params = req
      let DocumentFormattingParams _workDoneToken textDocument _formattingOptions = params
      let TextDocumentIdentifier uri = textDocument
      case uriToFilePath uri of
        Nothing ->
          responder (Left $ ResponseError InvalidParams "Cannot parse URI" Nothing)
        Just filePath -> do
          pythonFile <- liftIO $ openFile filePath ReadMode
          fileContents <- liftIO $ hGetContents pythonFile
          let prettifiedContent = prettifyFileContent (fromString fileContents)

          let fullDocumentRange = Range (Position 0 0) (getLastPosition (fromString fileContents))
          responder (Right $ List [TextEdit fullDocumentRange prettifiedContent])
  -- Handle go-to-symbol request
  , requestHandler STextDocumentDocumentSymbol $ \req responder -> do
      let RequestMessage _jsonrpc _id _method params = req
      let DocumentSymbolParams _workDoneToken _partialResultToken textDocument = params
      let TextDocumentIdentifier uri = textDocument
      case uriToFilePath uri of
        Nothing ->
          responder (Left $ ResponseError InvalidParams "Cannot parse URI" Nothing)
        Just filePath -> do
          pythonFile <- liftIO $ openFile filePath ReadMode
          fileContent <- liftIO $ hGetContents pythonFile
          case PyV3.parseModule fileContent filePath of
            Left _e -> responder
              (Left $ ResponseError InvalidParams "Failed to parse the Python module" Nothing)
            Right (ast, _comments) -> responder (Right (InR $ List $ getSymbols ast))
  -- Handle go-to-declaration request
  , requestHandler STextDocumentDeclaration $ \req responder -> do
      let RequestMessage _jsonrpc _id _method params = req
      let DeclarationParams textDocument position _workDoneToken _partialResultToken = params
      let TextDocumentIdentifier uri = textDocument
      cwd <- lift $ lift $ liftIO getCurrentDirectory
      allPythonFiles <- lift $ lift $ liftIO $ listFilesDirFiltered cwd
      let allASTs = fileContentsToASTs allPythonFiles
      let allSymbols = RIO.concatMap getSymbols (rights allASTs)
      case uriToFilePath uri of
        Nothing ->
          responder (Left $ ResponseError InvalidParams "Cannot parse URI" Nothing)
        Just filePath -> do
          pythonFile <- liftIO $ openFile filePath ReadMode
          fileContent <- liftIO $ hGetContents pythonFile
          case PyV3.parseModule fileContent filePath of
            Left _e -> responder
              (Left $ ResponseError InvalidParams "Failed to parse the Python module" Nothing)
            Right (ast, _comments) -> do
              case getASTNodeByPosition position ast of
                Nothing -> responder
                  (Left $ ResponseError InvalidParams "Invalid position" Nothing)
                Just ident -> do
                  case RIO.List.find (matchingIdentifier ident) allSymbols of
                    Nothing     -> responder (Left $ ResponseError InvalidParams "Nothing found" Nothing)
                    Just (SymbolInformation _ _ _ _ loc _) -> responder (Right $ InL loc)
  ]


{-| Get position of the last symbol in a file. -}
getLastPosition
  :: Text
  -> Position
getLastPosition fileContent = Position lineNum columnNum
  where
    splitContent = RIO.Text.lines fileContent
    lineNum = max (RIO.length splitContent - 1) 0
    columnNum = lastElementLength splitContent

    lastElementLength :: [Text] -> Int
    lastElementLength []      = 0
    lastElementLength [word]  = RIO.Text.length word
    lastElementLength (_x:xs) = lastElementLength xs


{-| Get AST node by its position. -}
getASTNodeByPosition
  :: Position
  -> PyAST.ModuleSpan
  -> Maybe (PyAST.Ident PythonSrcLoc.SrcSpan)
getASTNodeByPosition position ast = ident
  where
    allIdents = toListOf template ast :: [PyAST.Ident PythonSrcLoc.SrcSpan]
    ident = RIO.List.find (positionInSpan position . PyAST.ident_annot) allIdents


{-| Check if symbol is inside a range. -}
positionInSpan
  :: Position
  -> PythonSrcLoc.SrcSpan
  -> Bool
positionInSpan (Position line character) (PythonSrcLoc.SpanCoLinear _fileName row startCol endCol) =
  (line + 1) == row && startCol <= character && endCol >= character
positionInSpan (Position line character) (PythonSrcLoc.SpanMultiLine _fileName startRow startCol endRow endCol) =
  startRow <= (line + 1) && endRow >= (line + 1) && startCol <= character && endCol >= character
positionInSpan (Position line character) (PythonSrcLoc.SpanPoint _fileName row col) =
  (line + 1) == row && character == col
positionInSpan (Position _line _character) PythonSrcLoc.SpanEmpty =
  False


{-| Prettify file content (note: prettifier removes comments). -}
prettifyFileContent
  :: Text
  -> Text
prettifyFileContent fileContent = prettifierResult
  where
    parseResult = PyV3.parseModule (T.unpack fileContent) "untitled.py"
    prettifierResult = case parseResult of
      Left _e                   -> fileContent
      Right (newAST, _comments) -> fromString (PyCommon.prettyText newAST)


{-| Replace file content with new content. -}
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


{-| List all files in a given directory. -}
listAllFiles
  :: FilePath
  -> DirTree Text
  -> [(FilePath, Text)]
listAllFiles _root (Failed _ _) = []
listAllFiles root (File fileName fileContent) = [(joinPath [root, fileName], fileContent)]
listAllFiles root (Dir dirName dirContents) = RIO.concatMap (listAllFiles (joinPath [root, dirName])) dirContents


{-| List all .py files in a given directory. -}
listFilesDirFiltered
  :: FilePath
  -> IO [(FilePath, Text)]
listFilesDirFiltered folderPath = do
    root :/ tree <- readDirectory folderPath
    return $ listAllFiles root (fromString <$> filterDir pythonOnly tree)
  where
    pythonOnly (Dir _ _)                = True
    pythonOnly (File fileName _content) = takeExtension fileName == ".py"
    pythonOnly (Failed _ _)             = False


{-| Map each file content to AST. -}
fileContentsToASTs
  :: [(FilePath, Text)]
  -> [Either ErrorCode PyAST.ModuleSpan]
fileContentsToASTs [] = []
fileContentsToASTs ((filePath, fileContent) : restFiles) = do
  case PyV3.parseModule (T.unpack fileContent) filePath of
      Left _e                -> Left ParseError : fileContentsToASTs restFiles
      Right (ast, _comments) -> Right ast : fileContentsToASTs restFiles


{-| Match Python identifiers. -}
matchingIdentifier :: PyAST.Ident PythonSrcLoc.SrcSpan -> SymbolInformation -> Bool
matchingIdentifier (PyAST.Ident identName _annot) (SymbolInformation symbolName _ _ _ _ _) = fromString identName == symbolName
