{-# LANGUAGE RankNTypes #-}

module Action where

import Numeric.AD (Mode, diff, diffF)
import Numeric.Tools.Integration
import Types

-- freeParticleLagrangian :: Mass -> Lagrangian Double
-- freeParticleLagrangian (Mass m) = Lagrangian {l = \localTuple -> sum (squareVelocity (time localTuple) <$> velocity localTuple)}
--   where
--     squareVelocity :: Double -> (Double -> Double) -> Double
--     squareVelocity t v = 0.5 * m * (v t * v t)

fstFixed :: a -> (b -> (a, b))
fstFixed x y = (x, y)

sndFixed :: b -> (a -> (a, b))
sndFixed y x = (x, y)

d1 :: (Floating a) => (forall s. (Mode s) => (b, s) -> s) -> (b, a) -> a
d1 f' (x, y) = diff (f' . fstFixed x) y

d2 :: (Floating a) => (forall s. (Mode s) => (s, b) -> s) -> (b, a) -> a
d2 f' (x, y) = diff (f' . sndFixed x) y

expand :: (forall a. (Floating a) => a -> [a]) -> (T -> (T, Q, V))
expand w t = (t, w t, diffF w t)

-- action :: (forall a. Floating a => a -> [a]) -> ((T, Q, V) -> Double) -> T -> Double
-- action w lagrangian = lagrangian . expand w

action :: (forall a. (Floating a) => a -> [a]) -> ((T, Q, V) -> Double) -> T -> T -> Maybe Double
action w lagrangian t1 t2 = quadRes $ quadRomberg defQuad (t1, t2) (lagrangian . expand w)

freeParticleLagrangian :: Mass -> (T, Q, V) -> Double
freeParticleLagrangian (Mass m) (_, _, v) = 0.5 * m * sum ((^ 2) <$> v)

-- gamma :: [PathFunction] -> Double -> LocalTuple Double
-- gamma path t =
--   LocalTuple
--     { time = t,
--       position = applyPathFunction <$> path,
--       velocity = _diff <$> path
--     }
--   where
--     _diff :: PathFunction -> Double -> Double
--     _diff (PathFunction f) = diff f

-- action :: Lagrangian Double -> [PathFunction] -> Double -> Double -> Maybe Double
-- action lagrangian path t1 t2 = quadRes $ quadRomberg defQuad (t1, t2) (l lagrangian . gamma path)
