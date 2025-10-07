{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Curve.Build
  ( DiscountCurve (..)
  , CurvePillar (..)
  , CurveReport (..)
  , buildCurve
  , dfAt
  , zAt
  , forwardBetween
  ) where

import Control.Monad (unless)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Time (Day)
import GHC.Generics (Generic)

import Domain
import Data.ECB (CurvePoint (..))

-- | Internal representation for each tenor pillar.
data CurvePillar = CurvePillar
  { pillarTenor :: !TenorYears
  , pillarTenorValue :: !Double
  , pillarRate :: !RateCC
  , pillarDiscount :: !DiscountFactor
  , pillarLogDiscount :: !Double
  } deriving (Eq, Show, Generic)

-- | Fully built discount curve with interpolation policy baked in.
data DiscountCurve = DiscountCurve
  { dcAsOf :: !Day
  , dcConfig :: !CurveConfig
  , dcPillars :: !(NonEmpty CurvePillar)
  } deriving (Eq, Show)

-- | Summary metrics recorded for curve_report.json.
data CurveReport = CurveReport
  { crMonotoneDf :: !Bool
  , crMinDf :: !Double
  , crMaxDf :: !Double
  , crNegForwardShare :: !Double
  } deriving (Eq, Show)

-- | Entry point: convert raw curve points into an interpolated discount curve.
buildCurve
  :: CurveConfig
  -> [CurvePoint]
  -> Either DomainError (DiscountCurve, CurveReport)
buildCurve cfg rawPoints = do
  nonEmptyPoints <- maybe (Left EmptyCurve) Right (NE.nonEmpty sortedPoints)
  let asOf = cpDate (NE.head nonEmptyPoints)
  case mismatchedDates asOf (NE.toList nonEmptyPoints) of
    Nothing -> pure ()
    Just bad -> Left (InconsistentCurveDates asOf bad)
  unless (strictlyAscendingTenors nonEmptyPoints) (Left NonAscendingTenors)
  pillars <- traverse curvePointToPillar (NE.toList nonEmptyPoints)
  let pillarsNE = NE.fromList pillars
      monotone = monotoneDiscounts pillarsNE
  unless monotone (Left NonMonotoneDiscountFactors)
  let curve = DiscountCurve
        { dcAsOf = asOf
        , dcConfig = cfg
        , dcPillars = pillarsNE
        }
      report = buildReport pillarsNE monotone
  pure (curve, report)
  where
    sortedPoints = sortOn (unTenorYears . cpTenor) rawPoints

-- | Discount factor interpolation (piecewise linear in log-DF space).
dfAt :: DiscountCurve -> Double -> DiscountFactor
dfAt curve t
  | t <= 0 = DiscountFactor 1.0
  | otherwise = DiscountFactor (exp (logDfAt curve t))

-- | Spot zero rate implied by the curve at tenor @t@ (continuous compounding).
zAt :: DiscountCurve -> Double -> RateCC
zAt curve t
  | t <= 0 = RateCC 0
  | otherwise = RateCC (-(logDfAt curve t) / t)

-- | Instantaneous forward rate between two tenors (continuous compounding).
forwardBetween :: DiscountCurve -> Double -> Double -> Double
forwardBetween curve t1 t2
  | t2 <= t1 = 0
  | otherwise =
      let lnDf1 = logDfAt curve t1
          lnDf2 = logDfAt curve t2
       in -(lnDf2 - lnDf1) / (t2 - t1)

-- Internal -------------------------------------------------------------------

curvePointToPillar :: CurvePoint -> Either DomainError CurvePillar
curvePointToPillar CurvePoint{cpTenor, cpZeroRate} = do
  let t = unTenorYears cpTenor
      r = unRateCC cpZeroRate
      dfValue = exp (negate r * t)
  discount <- mkDiscountFactor dfValue
  let logDf = log dfValue
  pure CurvePillar
        { pillarTenor = cpTenor
        , pillarTenorValue = t
        , pillarRate = cpZeroRate
        , pillarDiscount = discount
        , pillarLogDiscount = logDf
        }

logDfAt :: DiscountCurve -> Double -> Double
logDfAt DiscountCurve{dcPillars} t =
  let pillars = NE.toList dcPillars
      origin = CurvePillar
        { pillarTenor = TenorYears 0
        , pillarTenorValue = 0
        , pillarRate = RateCC 0
        , pillarDiscount = DiscountFactor 1
        , pillarLogDiscount = 0
        }
      (before, after) = span ((< t) . pillarTenorValue) pillars
   in case (reverse before, after) of
        ([], next : _) -> interpolateLog origin next t
        (prev : _, next : _) -> interpolateLog prev next t
        (prev : _, []) -> extrapolateTail prev pillars t
        ([], []) -> 0 -- Should not occur

interpolateLog :: CurvePillar -> CurvePillar -> Double -> Double
interpolateLog leftP rightP t =
  let tL = pillarTenorValue leftP
      tR = pillarTenorValue rightP
      w = (t - tL) / (tR - tL)
      lnL = pillarLogDiscount leftP
      lnR = pillarLogDiscount rightP
   in lnL + w * (lnR - lnL)

extrapolateTail :: CurvePillar -> [CurvePillar] -> Double -> Double
extrapolateTail _lastWithin pillars t =
  let lastP = last pillars
      prevP = case reverse pillars of
        (_ : second : _) -> Just second
        [_] -> Nothing
        [] -> Nothing
      fwd = tailForwardRate lastP prevP
      tLast = pillarTenorValue lastP
      lnLast = pillarLogDiscount lastP
   in lnLast - fwd * (t - tLast)

tailForwardRate :: CurvePillar -> Maybe CurvePillar -> Double
tailForwardRate lastP Nothing = unRateCC (pillarRate lastP)
tailForwardRate lastP (Just prevP) = forwardBetweenPillars prevP lastP

forwardBetweenPillars :: CurvePillar -> CurvePillar -> Double
forwardBetweenPillars leftP rightP =
  let lnL = pillarLogDiscount leftP
      lnR = pillarLogDiscount rightP
      tL = pillarTenorValue leftP
      tR = pillarTenorValue rightP
   in -(lnR - lnL) / (tR - tL)

monotoneDiscounts :: NonEmpty CurvePillar -> Bool
monotoneDiscounts pillars =
  and $ zipWith (>=) dfs (tail dfs)
  where
    dfs = fmap (unDiscountFactor . pillarDiscount) (NE.toList pillars)

strictlyAscendingTenors :: NonEmpty CurvePoint -> Bool
strictlyAscendingTenors points =
  let tenors = fmap (unTenorYears . cpTenor) (NE.toList points)
   in case tenors of
        [] -> True
        [_] -> True
        _ -> and $ zipWith (<) tenors (tail tenors)

buildReport :: NonEmpty CurvePillar -> Bool -> CurveReport
buildReport pillars monotone =
  let dfs = fmap (unDiscountFactor . pillarDiscount) (NE.toList pillars)
      forwards = forwardRates pillars
      negShare = case forwards of
        [] -> 0
        _ -> fromIntegral (length (filter (< 0) forwards)) / fromIntegral (length forwards)
   in CurveReport
        { crMonotoneDf = monotone
        , crMinDf = minimum dfs
        , crMaxDf = maximum dfs
        , crNegForwardShare = negShare
        }

forwardRates :: NonEmpty CurvePillar -> [Double]
forwardRates pillars =
  case NE.toList pillars of
    [] -> []
    [_] -> []
    ps -> zipWith forwardBetweenPillars ps (tail ps)

mismatchedDates :: Day -> [CurvePoint] -> Maybe Day
mismatchedDates expected = go
  where
    go [] = Nothing
    go (point : rest)
      | cpDate point == expected = go rest
      | otherwise = Just (cpDate point)
