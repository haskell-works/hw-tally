{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Run
  ( cmdRun
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
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
runRun opt = void $ runExceptT $ do
  let ballotFile  = opt ^. the @"ballotFile"
  let votePath    = opt ^. the @"votePath"

  files <- liftIO $ IO.listDirectory votePath
  let voteFiles = (\f -> votePath <> "/" <> f) <$> filter (".yaml" `isSuffixOf`) files

  ballot  :: Z.Ballot <- liftIO (BS.readFile ballotFile) <&> Y.decodeEither >>= liftEither
  votes   :: [Z.Vote] <- sequence <$> forM voteFiles (fmap Y.decodeEither . liftIO . BS.readFile) >>= liftEither

  liftIO $ IO.print ballot
  liftIO $ IO.print votes

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
