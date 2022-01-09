-- |
-- Module      : PairChurchSpec
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- @'Spec'@ for @'PairChurch'@ as a valid @'Encoding'@
-- of @(,)@ values.
module Encoding.PairChurchSpec where

import           Encoding.PairChurch (ft, sd)
import           EncodingSpec        (EncodingSpec (..), EncodingTest (..),
                                      encodingSpec)
import           Test.Hspec          (Spec)

pairChurchSpec :: Spec
pairChurchSpec = encodingSpec EncodingSpec
  { name  = "PairChurch"
  , vals  = [(1 :: Int, False)]
  , tests =
      [ ("FT", PredicateTest fst ft)
      , ("SD", PredicateTest snd sd)
      ]
  }
