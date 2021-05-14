{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Symbols (getSymbols) where

import qualified Language.LSP.Types                 as LSPTypes
import qualified Language.Python.Common.AST         as PythonAST
import qualified Language.Python.Common.SrcLocation as PythonSrcLoc
import           RIO
import           RIO.List                           (repeat)


{-| Traverse a Python syntax tree and extract top-level 'Statement' nodes. -}
extractStatements :: PythonAST.ModuleSpan -> [PythonAST.Statement PythonSrcLoc.SrcSpan]
extractStatements (PythonAST.Module statements) = statements


findIdentifiersInExpr
  :: PythonAST.Expr PythonSrcLoc.SrcSpan
  -> [PythonAST.Ident PythonSrcLoc.SrcSpan]
findIdentifiersInExpr (PythonAST.Var varIdent _annot)        = [varIdent]
findIdentifiersInExpr (PythonAST.Tuple tupleExprs _annot)    = concatMap findIdentifiersInExpr tupleExprs
findIdentifiersInExpr (PythonAST.List listExprs _annot)      = concatMap findIdentifiersInExpr listExprs
findIdentifiersInExpr (PythonAST.Starred starredExpr _annot) = findIdentifiersInExpr starredExpr
findIdentifiersInExpr _anythingElse                          = []


{-| Recursively traverse a Statement and extract 'Ident' nodes
that correspond to defined symbols in the code. -}
extractIdentifiersFromStatement
  :: PythonAST.Statement PythonSrcLoc.SrcSpan
  -> [(PythonAST.Ident PythonSrcLoc.SrcSpan, LSPTypes.SymbolKind)]

extractIdentifiersFromStatement (PythonAST.Import importItems _annot) =
    zip (concatMap findIdentifiers importItems) (repeat LSPTypes.SkModule)
  where
    findIdentifiers (PythonAST.ImportItem [] Nothing _annot) = []
    findIdentifiers (PythonAST.ImportItem (rootPkg : _restPkgs) Nothing _annot) = [rootPkg]
    findIdentifiers (PythonAST.ImportItem _itemName (Just asName) _annot) = [asName]

extractIdentifiersFromStatement (PythonAST.FromImport _fromModule (PythonAST.ImportEverything _annot) _fromAnnot) = []
extractIdentifiersFromStatement (PythonAST.FromImport _fromModule (PythonAST.FromItems items _annot) _fromAnnot) =
    zip (concatMap findIdentifiers items) (repeat LSPTypes.SkModule)
  where
    findIdentifiers (PythonAST.FromItem name Nothing _annot) = [name]
    findIdentifiers (PythonAST.FromItem _itemName (Just asName) _annot) = [asName]

extractIdentifiersFromStatement (PythonAST.While _condition body elseBlock _annot) =
  concatMap extractIdentifiersFromStatement (body ++ elseBlock)

extractIdentifiersFromStatement (PythonAST.For forTargets _forGenerator body elseBlock _annot) =
  zip (concatMap findIdentifiersInExpr forTargets) (repeat LSPTypes.SkVariable)
  ++ concatMap extractIdentifiersFromStatement (body ++ elseBlock)

extractIdentifiersFromStatement (PythonAST.AsyncFor forLoop _annot) =
  extractIdentifiersFromStatement forLoop

extractIdentifiersFromStatement (PythonAST.Fun funName funArgs _result body _annot) =
    (function : args) ++ concatMap extractIdentifiersFromStatement body
  where
    function = (funName, LSPTypes.SkFunction)
    args = zip (concatMap findIdentifiers funArgs) (repeat LSPTypes.SkVariable )

    findIdentifiers (PythonAST.Param name _typeAnnot _default _annot) = [name]
    findIdentifiers (PythonAST.VarArgsPos name _typeAnnot _annot)     = [name]
    findIdentifiers (PythonAST.VarArgsKeyword name _typeAnnot _annot) = [name]
    findIdentifiers _anythingElse                                     = []

extractIdentifiersFromStatement (PythonAST.AsyncFun funDef _annot) =
  extractIdentifiersFromStatement funDef

extractIdentifiersFromStatement (PythonAST.Class name _args body _annot) =
  (name, LSPTypes.SkClass) : concatMap extractIdentifiersFromStatement body

extractIdentifiersFromStatement (PythonAST.Conditional guards elseBlock _annot) =
  concatMap extractIdentifiersFromStatement (elseBlock ++ concatMap snd guards)

extractIdentifiersFromStatement (PythonAST.Assign assignTo _assignExpr _annot) =
    zip (concatMap findIdentifiersInExpr assignTo) (repeat LSPTypes.SkVariable)


extractIdentifiersFromStatement _otherStatement = []


{-| Convert an 'Ident' node to a symbol for the LSP. -}
identToSymbol
  :: (PythonAST.Ident PythonSrcLoc.SrcSpan, LSPTypes.SymbolKind)
  -> Maybe LSPTypes.SymbolInformation
identToSymbol (identifier, kind) = conversionResult
  where
    PythonAST.Ident name annot = identifier
    locationMaybe = case annot of
        PythonSrcLoc.SpanCoLinear filename row startCol endCol ->
          Just $ locationFrom filename (row, startCol) (row, endCol)
        PythonSrcLoc.SpanMultiLine filename startRow startCol endRow endCol ->
          Just $ locationFrom filename (startRow, startCol) (endRow, endCol)
        PythonSrcLoc.SpanPoint filename row col ->
          Just $ locationFrom filename (row, col) (row, col)
        PythonSrcLoc.SpanEmpty ->
          Nothing
      where
        locationFrom filename (startRow, startCol) (endRow, endCol) =
          LSPTypes.Location
            (LSPTypes.filePathToUri filename)
            (LSPTypes.Range
              (LSPTypes.Position (startRow - 1) startCol)
              (LSPTypes.Position (endRow - 1) endCol)
            )

    conversionResult = case locationMaybe of
      Nothing -> Nothing
      Just location ->
        Just $ LSPTypes.SymbolInformation
          (fromString name)
          kind
          Nothing
          Nothing
          location
          Nothing


{-| Get symbols from a Python syntax tree. -}
getSymbols :: PythonAST.ModuleSpan -> [LSPTypes.SymbolInformation]
getSymbols = mapMaybe identToSymbol
  . concatMap extractIdentifiersFromStatement
  . extractStatements
