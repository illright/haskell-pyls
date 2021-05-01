{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Entrypoint (run) where

import           RIO   (RIO, logInfo)
import           Types (App)

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
