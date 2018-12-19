{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.Tally
  ( beginElection
  , stepElection
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.List
import Data.Map                  (Map)
import Data.Maybe
import Data.Set                  (Set)
import Data.Tuple

import qualified Data.Map                as M
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

voteExcluding :: Set Z.CandidateName -> Z.Vote -> Z.Vote
voteExcluding exclusions vote = vote & the @"preferences" %~ filter (`S.member` exclusions)

tallyVotes :: ()
  => [Z.Vote]
  -> Z.Ballot
  -> Set Z.CandidateName
  -> Map Z.CandidateName Double
tallyVotes votes ballot exclusions = M.unionsWith (+) $ do
  vote <- voteExcluding exclusions <$> votes
  topCandidate <- take 1 (vote ^. the @"preferences")
  return (M.singleton topCandidate (vote ^. the @"value"))

elect :: Z.Step -> Z.CandidateName -> Double -> Z.Step
elect step candidate value = step
  & the @"elected" %~ S.insert candidate
  & the @"votes"   .~ adjustedVoteValues -- TODO this adjustment is fake
  where adjustedVoteValues = step ^. the @"votes"

stepElection :: Z.Step -> Z.Step
stepElection step = case maybeTopChoice of
  Just (value, candidate) -> elect step candidate value
  Nothing                 -> step & the @"progress" .~ Z.Done
  where maybeTopChoice  = listToMaybe (sortBy (flip compare) (fmap swap (M.toList tally)))
        tally           = tallyVotes (step ^. the @"votes") (step ^. the @"ballot") exclusions
        exclusions      = step ^. the @"elected" <> step ^. the @"excluded" <> step ^. the @"deferred"
