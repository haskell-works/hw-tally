{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HaskellWorks.Tally.Json where

import Data.Aeson
import Data.Aeson.Parser
import Data.Aeson.Types

import qualified HaskellWorks.Tally.Type as Z

parseGender :: Value -> Parser Z.Gender
parseGender (String "female") = return Z.Female
parseGender _                 = return Z.NonFemale

parseVote :: Value -> Parser Z.Vote
parseVote = withObject "User" $ \o -> do
  value       <- o .: "vote"
  preferences <- o .: "preferences"
  spent       <- o .: "spent"
  history     <- o .: "history"
  return Z.Vote {..}
