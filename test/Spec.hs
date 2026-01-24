{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Action
import Test.Hspec (Spec, describe, hspec, it, shouldBe, shouldSatisfy)
import Types

main :: IO ()
main = hspec spec

-- linearPath :: Double -> Double -> PathFunction
-- linearPath slope intercept =
--   PathFunction (\t -> realToFrac slope * t + realToFrac intercept)

-- testPath :: [PathFunction]
-- testPath = zipWith linearPath [4, 3, 2] [7, 5, 1]

testPath :: forall a. (Floating a) => a -> [a]
testPath t = [4 * t + 7, 3 * t + 5, 2 * t + 1]

eta :: forall a. (Floating a) => (a -> [a]) -> a -> a -> (a -> [a])
eta w t1 t2 t = ((t - t1) * (t - t2) *) <$> w t

nu :: forall a. (Floating a) => a -> [a]
nu t = [sin t, cos t, t**2]

variedFPAction :: Floating a => (a -> [a]) -> a -> a -> a -> a -> [a]
variedFPAction w epsilon t1 t2 t = zipWith (+) (w t) $ (epsilon *) <$> eta nu t1 t2 t

spec :: Spec
spec = do
  describe "Lagrangian Action" $ do
    it "free particle Lagrangian on test path from t1=0 to t2=10" $ do
      action testPath (freeParticleLagrangian (Mass 3.0)) 0.0 10.0 `shouldBe` Just 435.0
    it "free particle Lagrangian on perturbed test path from t1=0 to t2=10" $ do
      action (variedFPAction testPath 0.001 0.0 10.0) (freeParticleLagrangian (Mass 3.0)) 0.0 10.0  `shouldSatisfy` maybe False (> 435)
