{-# LANGUAGE RecordWildCards #-}

module Pricing.Bond
  ( Bond (..)
  , CashFlow (..)
  , generateCashFlows
  , bondCashFlows
  , dirtyPrice
  , accruedInterest
  , cleanPrice
  ) where

import Data.List (foldl')
import Data.Scientific (fromFloatDigits)
import Data.Time (Day, addGregorianMonthsRollOver)

import Curve.Build (DiscountCurve, dfAt)
import Domain

-- | Fixed-rate bullet bond (MVP assumes annual coupons).
data Bond = Bond
  { bondId :: !String
  , bondSettlement :: !Day
  , bondMaturity :: !Day
  , bondCouponRate :: !RateCC
  , bondNotional :: !Money
  , bondFrequency :: !Int -- coupons per year (MVP: 1)
  } deriving (Eq, Show)

-- | Cash-flow entry with amount and year fraction from settlement.
data CashFlow = CashFlow
  { cfDate :: !Day
  , cfAmount :: !Money
  , cfYearFraction :: !Double
  } deriving (Eq, Show)

-- | Generate coupon + principal cash flows (pure schedule, no validation).
generateCashFlows :: Bond -> Either DomainError [CashFlow]
generateCashFlows Bond{..}
  | bondFrequency /= 1 = Left (UnsupportedCouponFrequency bondFrequency)
  | bondSettlement >= bondMaturity = Right []
  | otherwise = Right (reverse (go firstCoupon []))
  where
    couponAmount = scaleMoney bondNotional (unRateCC bondCouponRate / fromIntegral bondFrequency)
    firstCoupon = advance bondSettlement

    go :: Day -> [CashFlow] -> [CashFlow]
    go current acc
      | current >= bondMaturity =
          let finalAmount = addMoney couponAmount bondNotional
              yf = yearFractionAct365 bondSettlement bondMaturity
           in CashFlow bondMaturity finalAmount yf : acc
      | otherwise =
          let yf = yearFractionAct365 bondSettlement current
              flow = CashFlow current couponAmount yf
           in go (advance current) (flow : acc)

    advance day = addGregorianMonthsRollOver (fromIntegral monthsPerPeriod) day
    monthsPerPeriod = 12 `div` bondFrequency

-- | Cash flows wrapped with validation (alias for generateCashFlows).
bondCashFlows :: Bond -> Either DomainError [CashFlow]
bondCashFlows = generateCashFlows

-- | Dirty price = discounted sum of cash flows using supplied curve.
dirtyPrice :: DiscountCurve -> Bond -> Either DomainError Money
dirtyPrice curve bond = do
  cfs <- bondCashFlows bond
  let pv = foldl' addMoney (Money 0) (map present cfs)
  pure pv
  where
    present CashFlow{..} =
      let df = dfAt curve cfYearFraction
       in scaleMoney cfAmount (unDiscountFactor df)

-- | Accrued interest using ACT/365 linear accrual.
accruedInterest :: Bond -> Either DomainError Money
accruedInterest bond@Bond{..}
  | bondFrequency /= 1 = Left (UnsupportedCouponFrequency bondFrequency)
  | otherwise = do
      cfs <- bondCashFlows bond
      case cfs of
        [] -> Right (Money 0)
        (firstCf : _) ->
          let monthsPerPeriod = 12 `div` bondFrequency
              prevCoupon = addGregorianMonthsRollOver (negate (fromIntegral monthsPerPeriod)) (cfDate firstCf)
              frac = max 0 (yearFractionAct365 prevCoupon bondSettlement)
              accrual = unRateCC bondCouponRate * frac
          in Right (scaleMoney bondNotional accrual)

-- | Clean price = dirty price - accrued interest.
cleanPrice :: DiscountCurve -> Bond -> Either DomainError Money
cleanPrice curve bond = do
  dirty <- dirtyPrice curve bond
  accrual <- accruedInterest bond
  pure (subtractMoney dirty accrual)

-- Helpers --------------------------------------------------------------------

scaleMoney :: Money -> Double -> Money
scaleMoney (Money amt) factor = Money (amt * fromFloatDigits factor)

addMoney :: Money -> Money -> Money
addMoney (Money x) (Money y) = Money (x + y)

subtractMoney :: Money -> Money -> Money
subtractMoney (Money x) (Money y) = Money (x - y)
