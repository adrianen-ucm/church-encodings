-- |
-- Module      : ListChurchSpec
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- @'Spec'@ for @'ListChurch'@ as a valid @'Encoding'@
-- of @[]@ values.
module Encoding.ListChurchSpec where

import           Encoding.ListChurch (app, hd, isEmpty, nil)
import           EncodingSpec        (EncodingSpec (..), EncodingTest (..),
                                      encodingSpec)
import           Test.Hspec          (Spec)

listChurchSpec :: Spec
listChurchSpec = encodingSpec EncodingSpec
  { name  = "ListChurch"
  , vals  = [[], [1], [4, 5], [0..10 :: Int]]
  , tests =
      [ ("NIL", EqTest [] nil)
      , ("APP", BinaryTest (<>) app)
      , ("HD", PredicateTest safeHead hd)
      , ("ISEMPTY", PredicateTest null isEmpty)
      ]
  }
  where
    safeHead []    = Nothing
    safeHead (a:_) = Just a
