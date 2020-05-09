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
import Data.Text                 (Text)
import HaskellWorks.Tally
import Options.Applicative       hiding (columns)

import qualified App.Commands.Types               as Z
import qualified Data.ByteString                  as BS
import qualified Data.Map                         as M
import qualified Data.Text                        as T
import qualified Data.Yaml                        as Y
import qualified HaskellWorks.Tally.IO.ByteString as BS
import qualified HaskellWorks.Tally.Type          as Z
import qualified System.Directory                 as IO
import qualified System.IO                        as IO

{- HLINT ignore "Reduce duplication"    -}

decodeVote :: (FilePath, BS.ByteString) -> Either String (Text, Z.Preferences)
decodeVote (filePath, contents) = case Y.decodeEither contents of
  Right preferences -> Right (T.pack filePath, preferences)
  Left e            -> Left e

runRun :: Z.RunOptions -> IO ()
runRun opt = void $ runExceptT $ do
  let ballotFile  = opt ^. the @"ballotFile"
  let votePath    = opt ^. the @"votePath"

  ballot      :: Z.Ballot                 <- liftIO (BS.readFile ballotFile                           ) <&> Y.decodeEither  >>= liftEither
  preferences :: [(Text, Z.Preferences)]  <- liftIO (BS.readFilesInDir votePath (".yaml" `isSuffixOf`)) <&> mapM decodeVote >>= liftEither

  let votes = M.fromList $ (\v -> (v ^. the @"id", v)). uncurry mkVote <$> preferences
  let step0 = beginElection ballot votes

  liftIO $ IO.print ballot
  liftIO $ IO.print votes
  liftIO $ IO.print step0

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
