{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Run
  ( cmdRun
  ) where

import App.Commands.Types
import Control.Lens
import Data.Maybe
import Data.Semigroup      ((<>))
import Data.Word
import Foreign
import Options.Applicative hiding (columns)

import qualified System.IO as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

runRun :: RunOptions -> IO ()
runRun _ = return ()

optsRun :: Parser RunOptions
optsRun = RunOptions
  <$> strOption
        (   long "directory"
        <>  short 'd'
        <>  help "Election directory"
        <>  metavar "STRING"
        )

cmdRun :: Mod CommandFields (IO ())
cmdRun = command "run"  $ flip info idm $ runRun <$> optsRun
