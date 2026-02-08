{-# LANGUAGE RankNTypes #-}

module Types where

import Numeric.AD (Mode)

newtype Mass = Mass Double deriving (Eq, Show)

newtype T = T {time :: forall a. Floating a => a}

type Q = [Double]

type V = [Double]

type Path = forall a. Floating a => a -> [a]

type LocalTuple a = (a, [a], [a])

type Lagrangian = forall s. (Floating s, Mode s) => LocalTuple s -> s
