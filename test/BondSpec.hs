module BondSpec (spec) where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Time (fromGregorian)
import Test.Hspec

import Curve.Build
import Data.ECB (CurvePoint (..))
import Domain
import Pricing.Bond

spec :: Spec
spec = describe "Pricing.Bond" $ do
  it "prices a zero-coupon bond equal to its discount factor" $ do
    case buildCurve defaultCurveConfig zeroCurve of
      Left err -> expectationFailure ("curve build failed: " <> show err)
      Right (curve, _) -> do
        runPricing curve zeroBond $ \price -> do
          let df = unDiscountFactor (dfAt curve maturityFrac)
              expected = moneyToDouble (bondNotional zeroBond) * df
          moneyToDouble price `shouldBeApprox` expected
        runAccrued zeroBond $ \accr -> moneyToDouble accr `shouldBeApprox` 0

  it "dirty price scales linearly with notional" $ do
    case buildCurve defaultCurveConfig sampleCurve of
      Left err -> expectationFailure ("curve build failed: " <> show err)
      Right (curve, _) -> do
        runPricing curve sampleBond $ \pvBase -> do
          let scaledBond = sampleBond { bondNotional = moneyScale (bondNotional sampleBond) 2 }
          runPricing curve scaledBond $ \pvScaled ->
            moneyToDouble pvScaled `shouldBeApprox` (2 * moneyToDouble pvBase)

-- Helpers --------------------------------------------------------------------

runPricing :: DiscountCurve -> Bond -> (Money -> Expectation) -> Expectation
runPricing curve bond k =
  case dirtyPrice curve bond of
    Left err -> expectationFailure ("pricing failed: " <> show err)
    Right pv -> k pv

runAccrued :: Bond -> (Money -> Expectation) -> Expectation
runAccrued bond k =
  case accruedInterest bond of
    Left err -> expectationFailure ("accrued interest failed: " <> show err)
    Right amt -> k amt

zeroBond :: Bond
zeroBond = Bond
  { bondId = "ZERO"
  , bondSettlement = fromGregorian 2025 10 2
  , bondMaturity = fromGregorian 2027 10 2
  , bondCouponRate = RateCC 0
  , bondNotional = moneyFromDouble 1000
  , bondFrequency = 1
  }

sampleBond :: Bond
sampleBond = Bond
  { bondId = "SAMPLE"
  , bondSettlement = fromGregorian 2025 10 2
  , bondMaturity = fromGregorian 2030 10 2
  , bondCouponRate = RateCC 0.025
  , bondNotional = moneyFromDouble 1000
  , bondFrequency = 1
  }

zeroCurve :: [CurvePoint]
zeroCurve = [mkPoint 2.0 0.02]

sampleCurve :: [CurvePoint]
sampleCurve =
  [ mkPoint 1.0 0.02
  , mkPoint 5.0 0.025
  , mkPoint 10.0 0.03
  ]

mkPoint :: Double -> Double -> CurvePoint
mkPoint tenor rate =
  CurvePoint
    { cpDate = fromGregorian 2025 10 2
    , cpTenor = TenorYears tenor
    , cpZeroRate = RateCC rate
    }

maturityFrac :: Double
maturityFrac = yearFractionAct365 (bondSettlement zeroBond) (bondMaturity zeroBond)

moneyFromDouble :: Double -> Money
moneyFromDouble = Money . fromFloatDigits

moneyToDouble :: Money -> Double
moneyToDouble (Money sci) = toRealFloat sci

moneyScale :: Money -> Double -> Money
moneyScale m factor = moneyFromDouble (moneyToDouble m * factor)

shouldBeApprox :: Double -> Double -> Expectation
shouldBeApprox actual expected =
  abs (actual - expected) `shouldSatisfy` (< 1e-8)
