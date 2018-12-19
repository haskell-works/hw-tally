{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.Tally
  ( beginElection
  , stepElection
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.Set                  (Set)

import qualified Data.Set                as S
import qualified HaskellWorks.Tally.Type as Z

beginElection :: Z.Ballot -> [Z.Vote] -> Z.Step
beginElection ballot votes = Z.Step
  { Z.round         = 0
  , Z.distribution  = 0
  , Z.ballot        = ballot
  , Z.votes         = votes
  , Z.elected       = S.empty
  , Z.excluded      = S.empty
  , Z.deferred      = S.empty
  , Z.quota         = 0.0
  , Z.results       = []
  , Z.annotation    = "Begin election"
  , Z.progress      = Z.Ongoing
  }

voteExcluding :: Z.Vote -> Set Z.CandidateName -> Z.Vote
voteExcluding vote exclusions = vote & the @"preferences" %~ filter (`S.member` exclusions)

tallyVotes :: ()
  => [Z.Vote]
  -> Z.Ballot
  -> Set Z.CandidateName
  -> Z.CandidateName
tallyVotes votes ballot exclusions = undefined

stepElection :: Z.Step -> Z.Step
stepElection step = step
