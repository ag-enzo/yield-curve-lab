{-# LANGUAGE RecordWildCards #-}

module Risk.Measures
  ( dv01Parallel
  , keyRateDV01
  , macaulayDuration
  , modifiedDuration
  , convexity
  ) where

import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Scientific (fromFloatDigits, toRealFloat)

import Curve.Build
import Domain
import Pricing.Bond

bpSize :: Double
bpSize = 1e-4

-- | Parallel DV01: price sensitivity to a +1bp shift in all spot zeros.
dv01Parallel :: DiscountCurve -> Bond -> Either DomainError Money
dv01Parallel curve bond = do
  basePrice <- dirtyPrice curve bond
  let bumpedCurve = shiftCurve bpSize curve
  bumpedPrice <- dirtyPrice bumpedCurve bond
  let baseVal = moneyToDouble basePrice
      bumpedVal = moneyToDouble bumpedPrice
      dv = -(bumpedVal - baseVal) / bpSize
  pure (moneyFromDouble dv)

-- | Key-rate DV01s using triangular weighting around each key tenor.
keyRateDV01 :: DiscountCurve -> Bond -> [TenorYears] -> Either DomainError [(TenorYears, Money)]
keyRateDV01 curve bond keys = do
  basePrice <- dirtyPrice curve bond
  let baseVal = moneyToDouble basePrice
  traverse (krdFor baseVal) keys
  where
    krdFor baseVal keyTenor = do
      let key = unTenorYears keyTenor
          bumped = bumpKey key curve
      bumpedPrice <- dirtyPrice bumped bond
      let bumpedVal = moneyToDouble bumpedPrice
          dv = -(bumpedVal - baseVal) / bpSize
      pure (keyTenor, moneyFromDouble dv)

-- | Macaulay duration (years).
macaulayDuration :: DiscountCurve -> Bond -> Either DomainError Double
macaulayDuration curve bond = do
  cfs <- bondCashFlows bond
  let pvComponents = map (present curve) cfs
      pvTotal = sum (map snd pvComponents)
  if pvTotal == 0
    then pure 0
    else pure (sum [ t * pv | (t, pv) <- pvComponents ] / pvTotal)

-- | Modified duration (continuous compounding ⇒ equals Macaulay).
modifiedDuration :: DiscountCurve -> Bond -> Either DomainError Double
modifiedDuration curve bond = macaulayDuration curve bond

-- | Convexity (continuous compounding approximation).
convexity :: DiscountCurve -> Bond -> Either DomainError Double
convexity curve bond = do
  cfs <- bondCashFlows bond
  let pvComponents = map (present curve) cfs
      pvTotal = sum (map snd pvComponents)
  if pvTotal == 0
    then pure 0
    else pure (sum [ t * t * pv | (t, pv) <- pvComponents ] / pvTotal)

-- Helpers --------------------------------------------------------------------

present :: DiscountCurve -> CashFlow -> (Double, Double)
present curve CashFlow{..} =
  let df = unDiscountFactor (dfAt curve cfYearFraction)
      amt = moneyToDouble cfAmount
   in (cfYearFraction, amt * df)

moneyToDouble :: Money -> Double
moneyToDouble (Money sci) = toRealFloat sci

moneyFromDouble :: Double -> Money
moneyFromDouble = Money . fromFloatDigits

-- Shift all zero rates by a constant (in continuous space).
shiftCurve :: Double -> DiscountCurve -> DiscountCurve
shiftCurve bump DiscountCurve{..} =
  let adjust pillar =
        let t = pillarTenorValue pillar
            newRate = RateCC (unRateCC (pillarRate pillar) + bump)
            logDf = -(unRateCC newRate) * t
         in pillar
              { pillarRate = newRate
              , pillarDiscount = DiscountFactor (exp logDf)
              , pillarLogDiscount = logDf
              }
      pillars' = fmap adjust dcPillars
   in DiscountCurve
        { dcAsOf = dcAsOf
        , dcConfig = dcConfig
        , dcPillars = pillars'
        }

bumpKey :: Double -> DiscountCurve -> DiscountCurve
bumpKey key DiscountCurve{..} =
  let pillarsList = NE.toList dcPillars
      weights = fmap (weightForKey key pillarsList) dcPillars
      bumped = NE.zipWith applyBump dcPillars weights
   in DiscountCurve
        { dcAsOf = dcAsOf
        , dcConfig = dcConfig
        , dcPillars = bumped
        }

applyBump :: CurvePillar -> Double -> CurvePillar
applyBump pillar weight =
  let bump = weight * bpSize
      t = pillarTenorValue pillar
      newRate = RateCC (unRateCC (pillarRate pillar) + bump)
      logDf = -(unRateCC newRate) * t
   in pillar
        { pillarRate = newRate
        , pillarDiscount = DiscountFactor (exp logDf)
        , pillarLogDiscount = logDf
        }

weightForKey :: Double -> [CurvePillar] -> CurvePillar -> Double
weightForKey key pillars pillar =
  let tenors = map pillarTenorValue pillars
      minT = head tenors
      maxT = last tenors
      t = pillarTenorValue pillar
      epsilon = 1e-12
      approxEquals a b = abs (a - b) < epsilon
  in if key <= minT then if approxEquals t minT then 1 else 0
     else if key >= maxT then if approxEquals t maxT then 1 else 0
     else
       case find (approxEquals key) tenors of
         Just matched -> if approxEquals t matched then 1 else 0
         Nothing ->
           let leftCandidates = takeWhile (< key) tenors
               rightCandidates = dropWhile (<= key) tenors
               leftT = last leftCandidates
               rightT = head rightCandidates
               spanWidth = rightT - leftT
            in if spanWidth <= 0
                 then 0
                 else if approxEquals t leftT
                        then (rightT - key) / spanWidth
                        else if approxEquals t rightT
                               then (key - leftT) / spanWidth
                               else 0
