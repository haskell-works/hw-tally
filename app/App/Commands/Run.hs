{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.Run
  ( cmdRun
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.Semigroup            ((<>))
import Options.Applicative       hiding (columns)

import qualified App.Commands.Types as Z
import qualified Data.Text.IO       as T

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

runRun :: Z.RunOptions -> IO ()
runRun opt = do
  let directory = opt ^. the @"directory"
  let electionFile = directory <> "/election"

  contents <- T.readFile electionFile

  T.putStr contents

  return ()

optsRun :: Parser Z.RunOptions
optsRun = Z.RunOptions
  <$> strOption
        (   long "directory"
        <>  short 'd'
        <>  help "Election directory"
        <>  metavar "STRING"
        )

cmdRun :: Mod CommandFields (IO ())
cmdRun = command "run"  $ flip info idm $ runRun <$> optsRun
