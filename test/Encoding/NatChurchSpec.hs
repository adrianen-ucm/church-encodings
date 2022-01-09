-- |
-- Module      : NatChurchSpec
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- @'Spec'@ for @'NatChurch'@ as a valid @'Encoding'@
-- of @'Natural'@ numbers.
--
-- It only checks up to the number 5 due to the cost
-- of exponentiation in Church encoded numbers.
module Encoding.NatChurchSpec where

import           Encoding.NatChurch (add, pow, mul, pre, sub)
import           EncodingSpec       (EncodingSpec (..), EncodingTest (..),
                                     encodingSpec)
import           Test.Hspec         (Spec)

natChurchSpec :: Spec
natChurchSpec = encodingSpec EncodingSpec
  { name  = "NatChurch"
  , vals  = [0..5]
  , tests =
      [ ("ADD", BinaryTest (+) add)
      , ("MUL", BinaryTest (*) mul)
      , ("POW", BinaryTest (^) pow)
      , ("PRE", UnaryTest safePred pre)
      , ("SUB", BinaryTest safeSub sub)
      ]
  }
  where
    safePred 0 = 0
    safePred n = pred n
    safeSub m n
      | n <= m = m - n
      | otherwise = 0
