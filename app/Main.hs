module Main where

import HaskellWorks.Tally.Type as Z

loadElection :: FilePath -> IO Z.Election
loadElection electionFilename = do
  return Z.Election
    {

    }

loadVotes :: IO [Z.Vote]
loadVotes = return []

main :: IO ()
main = do
  return ()
