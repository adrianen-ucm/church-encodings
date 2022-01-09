{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types             #-}

-- |
-- Module      : BoolChurch
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module defines a Church encoding
-- for @'Bool'@ values.
module Encoding.BoolChurch
  ( BoolChurch
  , boolChurch
  , boolUnchurch
  , neg
  , conj
  , disj
  ) where

import           Encoding (Encoding (from, to))

-- The following pragma avoids HLint hints
-- for simplifying our anonymous functions.
-- In this way we can write more clearly
-- anonymous functions which look like
-- lambda abstractions without HLint complains.
{-# ANN module "HLint: ignore" #-}

-- | The Church encoding type of a @'Bool'@ as
-- an opaque type, just to be seen as a @'Bool'@
-- alternative implementation in terms of polymorphic
-- lambda calculus terms.
newtype BoolChurch = BoolChurch
  (forall a. a -> a -> a)

-- | Instance for @'BoolChurch'@ to be an
-- encoding of @'Bool'@ values.
instance Encoding Bool BoolChurch where
  from = boolChurch
  to = boolUnchurch

-- | Encodes a @'Bool'@ value into its
-- corresponding @'BoolChurch'@.
boolChurch :: Bool -> BoolChurch
boolChurch True  = BoolChurch $ \x _ -> x
boolChurch False = BoolChurch $ \_ y -> y

-- | Recovers the @'Bool'@ value from a
-- @'BoolChurch'@.
boolUnchurch :: BoolChurch -> Bool
boolUnchurch (BoolChurch b) = b True False

-- | Logical negation of a @'BoolChurch'@.
neg :: BoolChurch -> BoolChurch
neg (BoolChurch b) =
  BoolChurch $ \x y -> b y x

-- | Logical conjunction between @'BoolChurch'@
-- values.
conj :: BoolChurch -> BoolChurch -> BoolChurch
conj (BoolChurch b) (BoolChurch c) =
  BoolChurch $ \x y -> b (c x y) y

-- | Logical disjunction between @'BoolChurch'@
-- values.
disj :: BoolChurch -> BoolChurch -> BoolChurch
disj (BoolChurch b) (BoolChurch c) =
  BoolChurch $ \x y -> b x (c x y)
