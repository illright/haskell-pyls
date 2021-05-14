{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Symbols (getSymbols) where

import           Control.Lens                       (toListOf)
import           Data.Data.Lens                     (template)
import           Data.List.Unique                   (sortUniq)
import qualified Language.LSP.Types                 as LSPTypes
import qualified Language.Python.Common.AST         as PythonAST
import qualified Language.Python.Common.SrcLocation as PythonSrcLoc
import           RIO


{-| Traverse a Python syntax tree and extract top-level 'Statement' nodes. -}
extractStatements :: PythonAST.ModuleSpan -> [PythonAST.Statement PythonSrcLoc.SrcSpan]
extractStatements = toListOf template


{-| Recursively traverse a Statement and extract 'Ident' nodes
that correspond to defined symbols in the code. -}
extractIdentifiersFromStatement
  :: PythonAST.Statement PythonSrcLoc.SrcSpan
  -> [PythonAST.Ident PythonSrcLoc.SrcSpan]
extractIdentifiersFromStatement (PythonAST.Assign assignTo _assignExpr _annot) =
    concatMap findIdentifiers assignTo
  where
    findIdentifiers (PythonAST.Var varIdent _annot)        = [varIdent]
    findIdentifiers (PythonAST.Tuple tupleExprs _annot)    = concatMap findIdentifiers tupleExprs
    findIdentifiers (PythonAST.List listExprs _annot)      = concatMap findIdentifiers listExprs
    findIdentifiers (PythonAST.Starred starredExpr _annot) = findIdentifiers starredExpr
    findIdentifiers _anythingElse                          = []
extractIdentifiersFromStatement _otherStatement = []


{-| A wrapper type defining equality and ordering of 'Ident'
nodes based on the text content of the identifier. -}
newtype IdentifierComparison = IdentifierComparison
  { getIdentifier :: PythonAST.Ident PythonSrcLoc.SrcSpan
  } deriving (Show)

instance Eq IdentifierComparison where
  a == b = aName == bName
    where
      IdentifierComparison (PythonAST.Ident aName _aAnnot) = a
      IdentifierComparison (PythonAST.Ident bName _bAnnot) = b

instance Ord IdentifierComparison where
  compare a b = compare aName bName
    where
      IdentifierComparison (PythonAST.Ident aName _aAnnot) = a
      IdentifierComparison (PythonAST.Ident bName _bAnnot) = b


{-| Remove duplicates from a list of 'Ident' nodes. -}
deduplicate :: [PythonAST.Ident PythonSrcLoc.SrcSpan] -> [PythonAST.Ident PythonSrcLoc.SrcSpan]
deduplicate = fmap getIdentifier . sortUniq . fmap IdentifierComparison


{-| Convert an 'Ident' node to a Variable symbol for the LSP. -}
identToVariableSymbol :: PythonAST.Ident PythonSrcLoc.SrcSpan -> Maybe LSPTypes.SymbolInformation
identToVariableSymbol identifier = conversionResult
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
          LSPTypes.SkVariable
          Nothing
          Nothing
          location
          Nothing


{-| Get symbols from a Python syntax tree. -}
getSymbols :: PythonAST.ModuleSpan -> [LSPTypes.SymbolInformation]
getSymbols = mapMaybe identToVariableSymbol
  . deduplicate
  . concatMap extractIdentifiersFromStatement
  . extractStatements
