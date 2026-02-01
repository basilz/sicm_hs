{-# LANGUAGE RankNTypes #-}

module ActionSpec(spec) where

import Action
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

testPathFreeParticle :: forall a. (Floating a) => a -> [a]
testPathFreeParticle t = [4 * t + 7, 3 * t + 5, 2 * t + 1]

testPathHarmonicOscillator :: forall a. (Floating a) => a -> [a]
testPathHarmonicOscillator t = [5.0 * cos (sqrt (1.5 / 3.0) * t + pi)]

eta :: forall a. (Floating a) => (a -> [a]) -> a -> a -> (a -> [a])
eta w t1 t2 t = ((t - t1) * (t - t2) *) <$> w t

nu :: forall a. (Floating a) => a -> [a]
nu t = [sin t, cos t, t ** 2]

variedFPAction :: (Floating a) => (a -> [a]) -> a -> a -> a -> a -> [a]
variedFPAction w epsilon t1 t2 t = zipWith (+) (w t) $ (epsilon *) <$> eta nu t1 t2 t

spec :: Spec
spec = do
  describe "Lagrangian Action" $ do
    it "free particle Lagrangian on test path from t1=0 to t2=10" $ do
      action testPathFreeParticle (freeParticleLagrangian 3.0) 0.0 10.0 `shouldBe` Just 435.0
    it "free particle Lagrangian on perturbed test path from t1=0 to t2=10" $ do
      action (variedFPAction testPathFreeParticle 0.001 0.0 10.0) (freeParticleLagrangian 3.0) 0.0 10.0 `shouldSatisfy` maybe False (> 435)
    it "harmonic oscillator Lagrangian on test path from t1=0 to t2=10" $ do
      action testPathHarmonicOscillator (harmonicOscillatorLagrangian 3.0 1.5) 0.0 10.0 `shouldSatisfy` maybe False (\v -> abs (v - 0.0) < 0.0000001)
    it "Lagrange equations for the free particle on the test path" $ do
      sum (lagrangeEquation testPathFreeParticle (freeParticleLagrangian 3.0) (1.0 :: Double)) `shouldBe` 0.0
    it "Lagrange equations for the free particle on the varied path" $ do
      sum (lagrangeEquation (variedFPAction testPathFreeParticle 0.01 0.0 10.0) (freeParticleLagrangian 3.0) (1.0 :: Double)) `shouldSatisfy` (/= 0.0)
    it "Lagrange equations for the harmonic oscillator on the test path" $ do
      sum (lagrangeEquation testPathHarmonicOscillator (harmonicOscillatorLagrangian 3.0 1.5) (1.0 :: Double)) `shouldSatisfy` (\v -> abs (v - 0.0) < 0.0000001)
