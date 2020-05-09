{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.TallySpec (spec) where

import Control.Applicative
import Data.Char                   (ord)
import Data.Either
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Text.ParserCombinators.ReadP as RP

{- HLINT ignore "Redundant do"    -}

spec :: Spec
spec = describe "HaskellWorks.TallySpec" $ do
  it "stub" $ requireTest $ do
    True === True
