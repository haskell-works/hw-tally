{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Run
  ( cmdRun
  ) where

import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.List                 (isSuffixOf)
import Data.Semigroup            ((<>))
import Options.Applicative       hiding (columns)

import qualified App.Commands.Types      as Z
import qualified Data.ByteString         as BS
import qualified Data.Yaml               as Y
import qualified HaskellWorks.Tally.Type as Z
import qualified System.Directory        as IO
import qualified System.IO               as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

runRun :: Z.RunOptions -> IO ()
runRun opt = do
  let ballotFile  = opt ^. the @"ballotFile"
  let votePath    = opt ^. the @"votePath"

  files <- IO.listDirectory votePath
  let voteFiles = filter (".yaml" `isSuffixOf`) files

  ballot :: Either String Z.Ballot <- Y.decodeEither <$> BS.readFile ballotFile

  IO.print ballot

  forM_ voteFiles $ \voteFile -> do
    IO.putStrLn voteFile

  -- IO.print vote

optsRun :: Parser Z.RunOptions
optsRun = Z.RunOptions
  <$> strOption
        (   long "ballot-file"
        <>  short 'b'
        <>  help "Ballot File"
        <>  metavar "FILENAME"
        )
  <*> strOption
        (   long "vote-path"
        <>  short 'v'
        <>  help "Vote Path"
        <>  metavar "DIR"
        )

cmdRun :: Mod CommandFields (IO ())
cmdRun = command "run"  $ flip info idm $ runRun <$> optsRun
