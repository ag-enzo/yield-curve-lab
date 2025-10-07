module RiskSpec (spec) where

import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Time (fromGregorian)
import Test.Hspec

import Curve.Build
import Data.ECB (CurvePoint (..))
import Domain
import Pricing.Bond
import Risk.Measures

spec :: Spec
spec = describe "Risk.Measures" $ do
  it "DV01 matches finite difference" $ do
    curve <- buildCurve' sampleCurvePoints
    let bond = sampleBond
    dv <- expectRight (dv01Parallel curve bond)
    fd <- finiteDifference curve bond
    moneyToDouble dv `shouldBeApprox` fd

  it "Zero-coupon Macaulay equals maturity" $ do
    curve <- buildCurve' zeroCurvePoints
    let bond = zeroBond
    dur <- expectRight (macaulayDuration curve bond)
    dur `shouldBeApprox` zeroMaturity
    modDur <- expectRight (modifiedDuration curve bond)
    modDur `shouldBeApprox` zeroMaturity

  it "Zero-coupon convexity equals maturity squared" $ do
    curve <- buildCurve' zeroCurvePoints
    let bond = zeroBond
    conv <- expectRight (convexity curve bond)
    conv `shouldBeApprox` (zeroMaturity * zeroMaturity)

  it "Sum of KRDs approximates DV01" $ do
    curve <- buildCurve' sampleCurvePoints
    let bond = sampleBond
        keys = fmap TenorYears [1.0, 5.0, 10.0]
    dv <- expectRight (dv01Parallel curve bond)
    krds <- expectRight (keyRateDV01 curve bond keys)
    let sumKrd = sum [ moneyToDouble m | (_, m) <- krds ]
        dvVal = moneyToDouble dv
        diff = abs (dvVal - sumKrd)
    diff `shouldSatisfy` (< abs dvVal * 0.05)

-- Helpers --------------------------------------------------------------------

buildCurve' :: [CurvePoint] -> IO DiscountCurve
buildCurve' pts =
  case buildCurve defaultCurveConfig pts of
    Left err -> expectationFailure ("curve build failed: " <> show err) >> error "unreachable"
    Right (curve, _) -> pure curve

finiteDifference :: DiscountCurve -> Bond -> IO Double
finiteDifference curve bond = do
  base <- expectRight (dirtyPrice curve bond)
  let bumpedCurve = shiftCurveDeterministic curve bumpSize
  bumped <- expectRight (dirtyPrice bumpedCurve bond)
  let baseVal = moneyToDouble base
      bumpedVal = moneyToDouble bumped
  pure (-(bumpedVal - baseVal) / bumpSize)

bumpSize :: Double
bumpSize = 1e-4

shiftCurveDeterministic :: DiscountCurve -> Double -> DiscountCurve
shiftCurveDeterministic curve bump =
  let adjust pillar =
        let t = pillarTenorValue pillar
            newRate = RateCC (unRateCC (pillarRate pillar) + bump)
            logDf = -(unRateCC newRate) * t
         in pillar
              { pillarRate = newRate
              , pillarDiscount = DiscountFactor (exp logDf)
              , pillarLogDiscount = logDf
              }
   in curve { dcPillars = fmap adjust (dcPillars curve) }

sampleBond :: Bond
sampleBond = Bond
  { bondId = "SAMPLE"
  , bondSettlement = fromGregorian 2025 10 2
  , bondMaturity = fromGregorian 2030 10 2
  , bondCouponRate = RateCC 0.025
  , bondNotional = moneyFromDouble 1000000
  , bondFrequency = 1
  }

zeroBond :: Bond
zeroBond = Bond
  { bondId = "ZERO"
  , bondSettlement = fromGregorian 2025 10 2
  , bondMaturity = fromGregorian 2027 10 2
  , bondCouponRate = RateCC 0
  , bondNotional = moneyFromDouble 1000000
  , bondFrequency = 1
  }

zeroMaturity :: Double
zeroMaturity = yearFractionAct365 (bondSettlement zeroBond) (bondMaturity zeroBond)

sampleCurvePoints :: [CurvePoint]
sampleCurvePoints =
  [ mkPoint 1.0 0.02
  , mkPoint 5.0 0.025
  , mkPoint 10.0 0.03
  ]

zeroCurvePoints :: [CurvePoint]
zeroCurvePoints = [mkPoint zeroMaturity 0.02]

mkPoint :: Double -> Double -> CurvePoint
mkPoint tenor rate =
  CurvePoint
    { cpDate = fromGregorian 2025 10 2
    , cpTenor = TenorYears tenor
    , cpZeroRate = RateCC rate
    }

moneyFromDouble :: Double -> Money
moneyFromDouble = Money . fromFloatDigits

moneyToDouble :: Money -> Double
moneyToDouble (Money sci) = toRealFloat sci

expectRight :: Either DomainError a -> IO a
expectRight (Left err) = expectationFailure (show err) >> error "unreachable"
expectRight (Right x) = pure x

shouldBeApprox :: Double -> Double -> Expectation
shouldBeApprox actual expected =
  abs (actual - expected) `shouldSatisfy` (< 1e-8)
