{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Tally.Json where

import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types
import HaskellWorks.Tally.Type

parseGender :: Value -> Parser Gender
parseGender (String "female") = return Female
parseGender _                 = return NonFemale
