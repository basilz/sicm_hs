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
import Types

fstFixed :: (Mode s, Scalar s ~ a) => [a] -> a -> ([s] -> LocalTuple s)
fstFixed x t y = (auto t, fmap auto x, y)

sndFixed :: (Mode s, Scalar s ~ a) => [a] -> a -> ([s] -> LocalTuple s)
sndFixed y t x = (auto t, x, fmap auto y)

d1 :: (Floating a) => Lagrangian -> LocalTuple a -> [a]
d1 lagrangian (t, q, v) = grad (lagrangian . sndFixed v t) q

d2 :: (Floating a) => Lagrangian -> LocalTuple a -> [a]
d2 lagrangian (t, q, v) = grad (lagrangian . fstFixed q t) v

expand :: (Floating a, Mode a) => Path -> a -> LocalTuple a
expand (w) t = (t, w t, diffF w t)

action :: Path -> Lagrangian -> Double -> Double -> Maybe Double
action w lagrangian t1 t2 = quadRes $ quadRomberg defQuad (t1, t2) (lagrangian . expand w)

freeParticleLagrangian :: (Floating a, Mode a) => a -> LocalTuple a -> a
freeParticleLagrangian m (_, _, v) = 0.5 * m * sum ((** 2) <$> v)

harmonicOscillatorLagrangian :: (Floating a, Mode a) => a -> a -> LocalTuple a -> a
harmonicOscillatorLagrangian m k (_, q, v) = 0.5 * m * sum ((** 2) <$> v) - 0.5 * k * sum ((** 2) <$> q)

-- lagrangeEquation computes the Euler-Lagrange equations: d/dt(∂L/∂v) - ∂L/∂q = 0
lagrangeEquation :: (Floating s, Mode s) => Path -> Lagrangian -> s -> [s]
lagrangeEquation w lagrangian t = zipWith (-) (diffF (d2 lagrangian . expand w) t) ((d1 lagrangian . expand w) t)