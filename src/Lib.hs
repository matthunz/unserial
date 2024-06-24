{-# LANGUAGE GADTs #-}

module Lib
  (
  )
where

import Data.Aeson (ToJSON (toJSON), Value)

data Item d o a where
  IntI :: Item d o Int

data Codec d o a where
  PureS :: a -> Codec d o a
  ItemS :: (d -> a) -> Item d o a -> Codec d o a
  BindS :: Codec d o b -> (b -> Codec d o a) -> Codec d o a

intC :: (d -> Int) -> Codec d o Int
intC f = ItemS f IntI

instance Functor (Codec d o) where
  fmap f c = BindS c (pure . f)

instance Applicative (Codec d o) where
  pure = PureS
  cf <*> ca = BindS cf (`fmap` ca)

instance Monad (Codec d o) where
  (>>=) = BindS
