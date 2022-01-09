{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : EncodingSpec
-- Copyright   : (c) Adrián Enríquez Ballester, 2021
--
-- @'Spec'@ for checking an @'Encoding'@ in terms of
-- some provided values, predicates, unary and binary
-- operators.
module EncodingSpec where

import           Control.Monad (forM_)
import           Encoding      (BinaryOp, Encoding (from, to), Predicate,
                                UnaryOp, toBinary, toPredicate, toUnary)
import           Test.Hspec    (Spec, describe, it)

data EncodingSpec a b = EncodingSpec
  { name  :: String
  , vals  :: [a]
  , tests :: [(String, EncodingTest a b)]
  }

data EncodingTest a b where
  EqTest :: a -> b -> EncodingTest a b
  UnaryTest :: UnaryOp a -> UnaryOp b -> EncodingTest a b
  BinaryTest :: BinaryOp a -> BinaryOp b -> EncodingTest a b
  PredicateTest :: Eq c => Predicate a c -> Predicate b c -> EncodingTest a b

encodingSpec :: forall a b. (Eq a, Encoding a b)
  => EncodingSpec a b
  -> Spec
encodingSpec spec =
  describe (name spec) $ do
    it "Valid conversion" $ test conversionTest $ vals spec
    forM_ (tests spec) $ \(n, t) ->
      it ("Valid " <> n <> " implementation") $
        test t $ vals spec
  where
    conversionTest :: EncodingTest a b
    conversionTest = UnaryTest id $ from . to
    test (EqTest a b) _ = a == to b
    test (UnaryTest aop bop) as = and
          [ aop v == toUnary bop v
          | v <- as
          ]
    test (BinaryTest aop bop) as = and
          [ aop v1 v2 == toBinary bop v1 v2
          | v1 <- as
          , v2 <- as
          ]
    test (PredicateTest aop bop) as = and
          [ aop v == toPredicate bop v
          | v <- as
          ]
