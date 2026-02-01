{-# LANGUAGE RankNTypes #-}

module Types
  ( Mass (..),
    T,
    Q,
    V,
    Path, path
  )
where

newtype Mass = Mass Double deriving (Eq, Show)

type T = Double

type Q = [Double]

type V = [Double]

newtype Path = Path {path :: forall a. Floating a => a -> [a]}
