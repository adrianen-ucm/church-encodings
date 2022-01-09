-- |
-- Module      : BoolChurchSpec
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- @'Spec'@ for @'BoolChurch'@ as a valid @'Encoding'@
-- of @'Bool'@ values.
module Encoding.BoolChurchSpec where

import           Encoding.BoolChurch (conj, disj, neg)
import           EncodingSpec        (EncodingSpec (..), EncodingTest (..),
                                      encodingSpec)
import           Test.Hspec          (Spec)

boolChurchSpec :: Spec
boolChurchSpec = encodingSpec EncodingSpec
  { name  = "BoolChurch"
  , vals  = [True, False]
  , tests =
    [ ("NEG", UnaryTest not neg)
    , ("CONJ", BinaryTest (&&) conj)
    , ("DISJ", BinaryTest (||) disj)
    ]
  }
