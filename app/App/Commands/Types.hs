{-# LANGUAGE DeriveGeneric #-}

module App.Commands.Types
  ( RunOptions(..)
  ) where

import GHC.Generics

data RunOptions = RunOptions
  { ballotFile :: FilePath
  , votePath   :: FilePath
  } deriving (Eq, Show, Generic)
