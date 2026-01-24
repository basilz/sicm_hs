{-# LANGUAGE RankNTypes #-}

module Types
  ( Mass (..),
    T,
    Q,
    V,
    applyPathFunction,
  )
where

newtype Mass = Mass Double deriving (Eq, Show)

type T = Double

type Q = [Double]

type V = [Double]

newtype PathFunction = PathFunction
  { runPathFunction :: forall a. (Floating a) => a -> a
  }

applyPathFunction :: (Floating a) => PathFunction -> a -> a
applyPathFunction (PathFunction f) = f
