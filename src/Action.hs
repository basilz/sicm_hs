{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Action(
    action,
    freeParticleLagrangian,
    harmonicOscillatorLagrangian,
    lagrangeEquation
) where

import Numeric.AD (Mode, Scalar, auto, diffF, grad)
import Numeric.Tools.Integration
    ( defQuad, quadRomberg, QuadRes(quadRes) )

fstFixed :: (Mode s, Scalar s ~ a) => [a] -> a -> ([s] -> (s, [s], [s]))
fstFixed x t y = (auto t, fmap auto x, y)

sndFixed :: (Mode s, Scalar s ~ a) => [a] -> a -> ([s] -> (s, [s], [s]))
sndFixed y t x = (auto t, x, fmap auto y)

d1 :: (Floating a) => (forall s. (Floating s, Mode s) => (s, [s], [s]) -> s) -> (a, [a], [a]) -> [a]
d1 lagrangian (t, q, v) = grad (lagrangian . sndFixed v t) q

d2 :: (Floating a) => (forall s. (Floating s, Mode s) => (s, [s], [s]) -> s) -> (a, [a], [a]) -> [a]
d2 lagrangian (t, q, v) = grad (lagrangian . fstFixed q t) v

expand :: (Floating a, Mode a) => (forall a1. (Floating a1) => a1 -> [a1]) -> (a -> (a, [a], [a]))
expand w t = (t, w t, diffF w t)

action :: (forall a. (Floating a) => a -> [a]) -> ((Double, [Double], [Double]) -> Double) -> Double -> Double -> Maybe Double
action w lagrangian t1 t2 = quadRes $ quadRomberg defQuad (t1, t2) (lagrangian . expand w)

freeParticleLagrangian :: (Floating a, Mode a) => a -> (a, [a], [a]) -> a
freeParticleLagrangian m (_, _, v) = 0.5 * m * sum ((** 2) <$> v)

harmonicOscillatorLagrangian :: (Floating a, Mode a) => a -> a -> (a, [a], [a]) -> a
harmonicOscillatorLagrangian m k (_, q, v) = 0.5 * m * sum ((** 2) <$> v) - 0.5 * k * sum ((** 2) <$> q)

-- lagrangeEquation computes the Euler-Lagrange equations: d/dt(∂L/∂v) - ∂L/∂q = 0
lagrangeEquation :: (Floating s, Mode s) => (forall a1. (Floating a1) => a1 -> [a1]) -> (forall s1. (Floating s1, Mode s1) => (s1, [s1], [s1]) -> s1) -> s -> [s]
lagrangeEquation w lagrangian t = zipWith (-) (diffF (d2 lagrangian . expand w) t) ((d1 lagrangian . expand w) t)