{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types             #-}

-- |
-- Module      : PairChurch
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module defines a Church encoding
-- for @(,)@ (i.e. pair) values.
module Encoding.PairChurch
  ( PairChurch
  , pairChurch
  , pairUnchurch
  , ft
  , sd
  ) where

import           Encoding (Encoding (from, to))

-- The following pragma avoids HLint hints
-- for simplifying our anonymous functions.
-- In this way we can write more clearly
-- anonymous functions which look like
-- lambda abstractions without HLint complains.
{-# ANN module "HLint: ignore" #-}

-- | The Church encoding type of a @(,)@ as
-- an opaque type, just to be seen as a @(,)@
-- alternative implementation in terms of
-- polymorphic lambda calculus terms.
newtype PairChurch a b = PairChurch
  (forall c. (a -> b -> c) -> c)

-- | Instance for @'PairChurch'@ to be an
-- encoding of @(,)@ values.
instance Encoding (a, b) (PairChurch a b) where
  from = pairChurch
  to = pairUnchurch

-- | Encodes a @(,)@ value into its
-- corresponding @'PairChurch'@.
pairChurch :: (a, b) -> PairChurch a b
pairChurch (a, b) = PairChurch $ \p -> p a b

-- | Recovers the @(,)@ value from a
-- @'PairChurch'@.
pairUnchurch :: PairChurch a b -> (a, b)
pairUnchurch (PairChurch p) = p (,)

-- | Retrieve the first element of a @'PairChurch'@.
ft :: PairChurch a b -> a
ft (PairChurch p) = p (\x _ -> x)

-- | Retrieve the second element of a @'PairChurch'@.
sd :: PairChurch a b -> b
sd (PairChurch p) = p (\_ y -> y)
