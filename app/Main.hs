{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import           Entrypoint                 (run)
import           Options.Applicative.Simple
import qualified Paths_haskell_pyls
import           RIO
import           Types                      (App (..), Options (..))

{-| Parse the arguments from the command line.

Remember `--` in order to ensure that the arguments
go to the program and not to Stack:

  `stack exec haskell-pyls -- -v`

-}
parseArgs :: IO (Options, ())
parseArgs = simpleOptions
  $(simpleVersion Paths_haskell_pyls.version)
  "Header for command line arguments"
  "Program description, also for command line arguments"
  (Options
     <$> switch ( long "verbose"
               <> short 'v'
               <> help "Verbose output?"
                )
  )
  empty

main :: IO Int
main = do
  (options, ()) <- parseArgs
  logOptions <- logOptionsHandle stderr (optionsVerbose options)
  withLogFunc logOptions $ \logFunction ->
    let app = App
          { appLogFunc = logFunction
          , appOptions = options
          }
     in runRIO app run
