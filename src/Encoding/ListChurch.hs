{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types             #-}

-- |
-- Module      : ListChurch
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module defines a Church encoding
-- for @[]@ (i.e. list) values.
module Encoding.ListChurch
  ( ListChurch
  , listChurch
  , listUnchurch
  , nil
  , app
  , hd
  , isEmpty
  ) where

import           Encoding (Encoding (from, to))

-- The following pragma avoids HLint hints
-- for simplifying our anonymous functions.
-- In this way we can write more clearly
-- anonymous functions which look like
-- lambda abstractions without HLint complains.
{-# ANN module "HLint: ignore" #-}

-- | The Church encoding type of an @[]@ as
-- an opaque type, just to be seen as an @[]@
-- alternative implementation in terms of
-- polymorphic lambda calculus terms.
newtype ListChurch a = ListChurch
  (forall b. (a -> b -> b) -> b -> b)

-- | Instance for @'ListChurch'@ to be an
-- encoding of @[]@ values.
instance Encoding [a] (ListChurch a) where
  from = listChurch
  to = listUnchurch

-- | Encodes a @[]@ value into its
-- corresponding @'ListChurch'@.
listChurch :: [a] -> ListChurch a
listChurch as = ListChurch $ \c n -> foldr c n as

-- | Recovers the @[]@ value from a
-- @'ListChurch'@.
listUnchurch :: ListChurch a -> [a]
listUnchurch (ListChurch l) = l (:) []

-- | The empty @'ListChurch'@ value.
nil :: ListChurch a
nil = ListChurch $ \_ n -> n

-- | Concatenation of two @'ListChurch'@ values.
app :: ListChurch a -> ListChurch a -> ListChurch a
app (ListChurch l1) (ListChurch l2) =
  ListChurch $ \c n -> l1 c (l2 c n)

-- | The first element of a nonempty @'ListChurch'@.
--
-- We have represented the possibility of failure
-- wrapping the result in @'Maybe'@ instead
-- of making this function partial.
hd :: ListChurch a -> Maybe a
hd (ListChurch l) =
  l (\x _ -> Just x) Nothing

-- | Check if a @'ListChurch'@ is empty.
isEmpty :: ListChurch a -> Bool
isEmpty (ListChurch l) =
  l (\_ _ -> False) True
