{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      : Encoding
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- This module defines a typeclass to handle a type
-- which is an encoding of another one.
module Encoding
  ( Encoding(from, to)
  , UnaryOp
  , BinaryOp
  , Predicate
  , fromUnary
  , fromBinary
  , fromPredicate
  , toUnary
  , toBinary
  , toPredicate
  ) where

-- | Class of types b which are an encoding of another
-- type a. The encoding type determines uniquely its
-- original type via @FunctionalDependencies@.
class Encoding a b | b -> a where
  from :: a -> b
  to :: b -> a

-- | Type alias for an unary operator.
type UnaryOp a = a -> a

-- | Type alias for a binary operator.
type BinaryOp a = a -> a -> a

-- | Type alias for a predicate operator.
type Predicate a b = a -> b

-- | Transform an unary operator of a
-- type into an unary operator of its
-- encoding type.
fromUnary :: Encoding a b => UnaryOp a -> UnaryOp b
fromUnary f = from . f . to

-- | Transform a binary operator of a
-- type into a binary operator of its
-- encoding type.
fromBinary :: Encoding a b => BinaryOp a -> BinaryOp b
fromBinary f x y = from $ f (to x) (to y)

-- | Transform a predicate of a
-- type into a predicate of its
-- encoding type.
fromPredicate :: Encoding a b => Predicate a c -> Predicate b c
fromPredicate f = f . to

-- | Transform an unary operator of an
-- encoded type into an unary operator of its
-- original type.
toUnary :: Encoding a b => UnaryOp b -> UnaryOp a
toUnary f = to . f . from

-- | Transform a binary operator of an
-- encoded type into a binary operator of its
-- original type.
toBinary :: Encoding a b => BinaryOp b -> BinaryOp a
toBinary f x y = to $ f (from x) (from y)

-- | Transform a predicate of an
-- encoded type into a predicate of its
-- original type.
toPredicate :: Encoding a b => Predicate b c -> Predicate a c
toPredicate f = f . from
