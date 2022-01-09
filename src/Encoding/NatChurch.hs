{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types             #-}

-- |
-- Module      : NatChurch
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module defines a Church encoding
-- for @'Natural'@ numbers.
module Encoding.NatChurch
  ( NatChurch
  , natChurch
  , natUnchurch
  , add
  , mul
  , pow
  , pre
  , sub
  ) where

import           Encoding        (Encoding (from, to))
import           Numeric.Natural (Natural)

-- The following pragma avoids HLint hints
-- for simplifying our anonymous functions.
-- In this way we can write more clearly
-- anonymous functions which look like
-- lambda abstractions without HLint complains.
{-# ANN module "HLint: ignore" #-}

-- | The Church encoding type of a @'Natural'@ as
-- an opaque type, just to be seen as a @'Natural'@
-- alternative implementation in terms of
-- polymorphic lambda calculus terms.
newtype NatChurch = NatChurch
  (forall a. (a -> a) -> a -> a)

-- | Instance for @'NatChurch'@ to be an
-- encoding of @'Natural'@ numbers.
instance Encoding Natural NatChurch where
  from = natChurch
  to = natUnchurch

-- | Encodes a @'Natural'@ number into its
-- corresponding @'NatChurch'@.
natChurch :: Natural -> NatChurch
natChurch n = NatChurch $ \s z ->
  iterate s z !! fromIntegral n

-- | Recovers the @'Natural'@ number from a
-- @'NatChurch'@.
natUnchurch :: NatChurch -> Natural
natUnchurch (NatChurch f) = f (+ 1) 0

-- | Addition of two @'NatChurch'@ values.
add :: NatChurch -> NatChurch -> NatChurch
add (NatChurch m) (NatChurch n) =
  NatChurch $ \s z -> m s (n s z)

-- | Multiplication of two @'NatChurch'@ values.
mul :: NatChurch -> NatChurch -> NatChurch
mul (NatChurch m) (NatChurch n) =
  NatChurch $ \s -> m (n s)

-- | Exponentiation of two @'NatChurch'@ values.
pow :: NatChurch -> NatChurch -> NatChurch
pow (NatChurch m) (NatChurch n) =
  NatChurch $ n m

-- | Predecessor of a @'NatChurch'@ number.
pre :: NatChurch -> NatChurch
pre (NatChurch n) =
  NatChurch $ \s z -> (
    n
      (\p f -> f (p (\x y -> y x)) s)
      (\f -> f z (\x -> x))
  ) (\x _ -> x)

-- | Subtract a @'NatChurch'@ number to another.
sub :: NatChurch -> NatChurch -> NatChurch
sub (NatChurch m) (NatChurch n) =
    n pre (NatChurch m)
