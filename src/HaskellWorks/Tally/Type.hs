{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HaskellWorks.Tally.Type where

import Data.Map     (Map)
import Data.Set     (Set)
import Data.Text    (Text)
import GHC.Generics

data Gender = Female | NonFemale deriving (Eq, Ord, Show, Generic)

type CandidateName = Text

newtype CandidateInfo = CandidateInfo
  { gender :: Gender
  } deriving (Eq, Ord, Show, Generic)

data Vote = Vote
  { value       :: Double
  , preferences :: [CandidateName]
  , spent       :: Map CandidateName Double
  , history     :: [Text]
  } deriving (Eq, Show, Generic)

newtype Distributions = Distributions
  { votesByCandidate :: Map CandidateName [Vote]
  } deriving (Eq, Show, Generic)

data Election = Election
  { candidates :: Map CandidateName CandidateInfo
  , elected    :: Set CandidateName
  , excluded   :: Set CandidateName
  , deferred   :: Set CandidateName
  , votes      :: [Vote]
  , quota      :: Double
  , results    :: [CandidateName]
  , round      :: Int
  , history    :: [Text]
  , earlier    :: Election
  } deriving (Eq, Show, Generic)
