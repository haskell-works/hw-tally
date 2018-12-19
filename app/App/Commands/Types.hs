{-# LANGUAGE DeriveGeneric #-}

module App.Commands.Types
  ( RunOptions(..)
  ) where

import GHC.Generics

newtype RunOptions = RunOptions
  { directory     :: FilePath
  } deriving (Eq, Show, Generic)
