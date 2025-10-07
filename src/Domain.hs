{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain
  ( TenorYears (..)
  , mkTenorYears
  , RateCC (..)
  , mkRateCC
  , DiscountFactor (..)
  , mkDiscountFactor
  , Money (..)
  , Bps (..)
  , bpsToDecimal
  , Compounding (..)
  , DayCountConvention (..)
  , InterpolationMethod (..)
  , ExtrapolationPolicy (..)
  , CurveConfig (..)
  , defaultCurveConfig
  , DomainError (..)
  , YearFraction
  , yearFractionAct365
  ) where

import Data.Scientific (Scientific)
import qualified Data.Text as T
import Data.Time (Day, diffDays)

-- | Continuous tenor measured in years.
newtype TenorYears = TenorYears { unTenorYears :: Double }
  deriving stock (Eq, Ord, Show)

-- | Smart constructor ensuring a non-negative tenor.
mkTenorYears :: Double -> Either DomainError TenorYears
mkTenorYears t
  | t < 0 = Left (NonPositiveTenor t)
  | otherwise = Right (TenorYears t)

-- | Continuously compounded zero-coupon rate.
newtype RateCC = RateCC { unRateCC :: Double }
  deriving stock (Eq, Ord, Show)

mkRateCC :: Double -> Either DomainError RateCC
mkRateCC r
  | isNaN r = Left (InvalidNumeric "rate")
  | otherwise = Right (RateCC r)

-- | Discount factor corresponding to a maturity.
newtype DiscountFactor = DiscountFactor { unDiscountFactor :: Double }
  deriving stock (Eq, Ord, Show)

mkDiscountFactor :: Double -> Either DomainError DiscountFactor
mkDiscountFactor df
  | isNaN df = Left (InvalidNumeric "discount factor")
  | df <= 0 || df > 1 = Left (DiscountFactorOutOfRange df)
  | otherwise = Right (DiscountFactor df)

-- | Monetary amounts used for prices, notionals, etc.
newtype Money = Money { unMoney :: Scientific }
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Fractional)

-- | Basis-point representation (1 bp = 1e-4).
newtype Bps = Bps { unBps :: Double }
  deriving stock (Eq, Ord, Show)

bpsToDecimal :: Bps -> Double
bpsToDecimal (Bps x) = x * 1e-4

-- | Compounding basis used for curve interpretation.
data Compounding
  = Continuous
  deriving stock (Eq, Show)

-- | Day-count conventions supported by the system.
data DayCountConvention
  = Act365F
  deriving stock (Eq, Show)

-- | Interpolation schemes available for the discount curve.
data InterpolationMethod
  = LogDiscountLinear
  deriving stock (Eq, Show)

-- | Extrapolation policy beyond the last curve pillar.
data ExtrapolationPolicy
  = FlatForward
  | ClampToLast
  deriving stock (Eq, Show)

-- | Runtime configuration collected from CLI/inputs.
data CurveConfig = CurveConfig
  { cfgCompounding :: Compounding
  , cfgDayCount :: DayCountConvention
  , cfgInterpolation :: InterpolationMethod
  , cfgExtrapolation :: ExtrapolationPolicy
  , cfgKeyRateTenors :: [TenorYears]
  }
  deriving stock (Eq, Show)

-- | Default MVP configuration as defined in the planning docs.
defaultCurveConfig :: CurveConfig
defaultCurveConfig =
  CurveConfig
    { cfgCompounding = Continuous
    , cfgDayCount = Act365F
    , cfgInterpolation = LogDiscountLinear
    , cfgExtrapolation = FlatForward
    , cfgKeyRateTenors =
        [ TenorYears 2.0
        , TenorYears 5.0
        , TenorYears 10.0
        , TenorYears 30.0
        ]
    }

-- | Domain-level validation errors.
data DomainError
  = NonPositiveTenor Double
  | DiscountFactorOutOfRange Double
  | InvalidNumeric String
  | CsvParseError T.Text
  | NonAscendingTenors
  | EmptyCurve
  | InconsistentCurveDates Day Day
  | NonMonotoneDiscountFactors
  | UnsupportedCouponFrequency Int
  deriving stock (Eq, Show)

type YearFraction = Double

-- | ACT/365F year-fraction calculation.
yearFractionAct365 :: Day -> Day -> YearFraction
yearFractionAct365 start end = fromIntegral (diffDays end start) / 365.0
