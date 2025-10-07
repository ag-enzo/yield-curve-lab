module GoldenSpec (spec) where

import Control.Monad (when)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Scientific (Scientific)
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Test.Hspec

import Curve.Build
import Data.ECB (CurvePoint (..), loadCurveCsv)
import Domain
import Pricing.Bond
import Report
import Risk.Measures

spec :: Spec
spec = describe "Golden analytics" $ do
  it "reproduces analytics.csv for sample data" $ do
    curvePoints <- loadCurveCsv sampleCurveCsv >>= either (fail . show) pure
    (curve, _) <- either (fail . show) pure (buildCurve defaultCurveConfig curvePoints)
    bonds <- loadBonds sampleBondsCsv
    rows <- mapM (bondAnalytics curve defaultKeyRates) bonds
    let actual = renderAnalyticsCsv rows
    expected <- BL.readFile goldenAnalyticsCsv
    actual `shouldBe` expected

-- File paths -----------------------------------------------------------------

sampleCurveCsv :: FilePath
sampleCurveCsv = "data/ecb_sample_2025-10-02.csv"

sampleBondsCsv :: FilePath
sampleBondsCsv = "input/bonds.csv"

goldenAnalyticsCsv :: FilePath
goldenAnalyticsCsv = "test/golden/analytics_expected.csv"

-- Loading bonds ---------------------------------------------------------------

data BondRow = BondRow
  { brId :: !T.Text
  , brSettlement :: !Day
  , brMaturity :: !Day
  , brCoupon :: !Double
  , brNotional :: !Scientific
  , brFrequency :: !Int
  }

instance Csv.FromNamedRecord BondRow where
  parseNamedRecord m =
    BondRow
      <$> m Csv..: BS.pack "id"
      <*> (m Csv..: BS.pack "settle_date" >>= parseDayField "settle_date")
      <*> (m Csv..: BS.pack "maturity" >>= parseDayField "maturity")
      <*> m Csv..: BS.pack "coupon_rate"
      <*> m Csv..: BS.pack "notional"
      <*> m Csv..: BS.pack "frequency"

parseDayField :: String -> BS.ByteString -> Csv.Parser Day
parseDayField label bs =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (BS.unpack bs) of
    Just day -> pure day
    Nothing -> fail ("Invalid date in " <> label)

loadBonds :: FilePath -> IO [Bond]
loadBonds path = do
  bytes <- BL.readFile path
  (_, rows) <- either (fail . show) pure (Csv.decodeByName bytes)
  mapM toBond (V.toList rows)

-- Bond analytics --------------------------------------------------------------

toBond :: BondRow -> IO Bond
toBond BondRow{ brId = bid, brSettlement = settle, brMaturity = mat
              , brCoupon = coupon, brNotional = notional, brFrequency = freq } = do
  when (freq /= 1) (fail "frequency=1 required in MVP")
  pure Bond
    { bondId = T.unpack bid
    , bondSettlement = settle
    , bondMaturity = mat
    , bondCouponRate = RateCC coupon
    , bondNotional = Money notional
    , bondFrequency = freq
    }

bondAnalytics :: DiscountCurve -> [TenorYears] -> Bond -> IO AnalyticsRow
bondAnalytics curve krdKeys bond = do
  dirty <- either (fail . show) pure (dirtyPrice curve bond)
  accrued <- either (fail . show) pure (accruedInterest bond)
  clean <- either (fail . show) pure (cleanPrice curve bond)
  macDur <- either (fail . show) pure (macaulayDuration curve bond)
  modDur <- either (fail . show) pure (modifiedDuration curve bond)
  conv <- either (fail . show) pure (convexity curve bond)
  dv <- either (fail . show) pure (dv01Parallel curve bond)
  krdVals <- either (fail . show) pure (keyRateDV01 curve bond krdKeys)
  let krdMap = Map.fromList krdVals
      lookupKrd key = moneyToScientific (Map.findWithDefault (Money 0) key krdMap)
  pure AnalyticsRow
    { arId = T.pack (bondId bond)
    , arClean = moneyToScientific clean
    , arDirty = moneyToScientific dirty
    , arAccrued = moneyToScientific accrued
    , arYtmCc = Nothing
    , arModDuration = modDur
    , arMacaulayDuration = macDur
    , arConvexity = conv
    , arDv01 = moneyToScientific dv
    , arKrd2 = lookupKrd (TenorYears 2)
    , arKrd5 = lookupKrd (TenorYears 5)
    , arKrd10 = lookupKrd (TenorYears 10)
    , arKrd30 = lookupKrd (TenorYears 30)
    }

moneyToScientific :: Money -> Scientific
moneyToScientific (Money sci) = sci

defaultKeyRates :: [TenorYears]
defaultKeyRates = map TenorYears [2,5,10,30]
