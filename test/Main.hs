-- |
-- Module      : Main
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- Entry point of the test suite which checks
-- @'boolChurchSpec'@, @'listChurchSpec'@,
-- @'natChurchSpec'@ and @'pairChurchSpec'@.
module Main (main) where

import           Encoding.BoolChurchSpec (boolChurchSpec)
import           Encoding.ListChurchSpec (listChurchSpec)
import           Encoding.NatChurchSpec  (natChurchSpec)
import           Encoding.PairChurchSpec (pairChurchSpec)
import           Test.Hspec              (hspec)

main :: IO ()
main = hspec $ do
  boolChurchSpec
  listChurchSpec
  natChurchSpec
  pairChurchSpec
