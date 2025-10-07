module CurveBuildSpec (spec) where

import Data.Time (fromGregorian)
import Test.Hspec

import Curve.Build
import Data.ECB (CurvePoint (..))
import Domain

spec :: Spec
spec = describe "Curve.Build" $ do
  it "interpolates discount factors correctly" $ do
    case buildCurve defaultCurveConfig samplePoints of
      Left err -> expectationFailure ("curve build failed: " <> show err)
      Right (curve, _) -> do
        let df1 = unDiscountFactor (dfAt curve 1.0)
            expectedDf1 = exp (-0.021 * 1.0)
        df1 `shouldBeApprox` expectedDf1

  it "reports monotone discount factors" $ do
    case buildCurve defaultCurveConfig samplePoints of
      Left err -> expectationFailure ("curve build failed: " <> show err)
      Right (_, report) -> crMonotoneDf report `shouldBe` True

  it "rejects discount factors > 1" $ do
    let badPoints =
          [ mkPoint 0.5 0.020
          , mkPoint 1.0 (-0.050)
          ]
    buildCurve defaultCurveConfig badPoints
      `shouldBe` Left (DiscountFactorOutOfRange (exp (0.050 * 1.0)))

samplePoints :: [CurvePoint]
samplePoints =
  [ mkPoint 0.5 0.020
  , mkPoint 1.0 0.021
  , mkPoint 2.0 0.022
  ]

mkPoint :: Double -> Double -> CurvePoint
mkPoint tenor rate =
  CurvePoint
    { cpDate = fromGregorian 2025 10 2
    , cpTenor = TenorYears tenor
    , cpZeroRate = RateCC rate
    }

shouldBeApprox :: Double -> Double -> Expectation
shouldBeApprox actual expected =
  abs (actual - expected) `shouldSatisfy` (< 1e-9)
