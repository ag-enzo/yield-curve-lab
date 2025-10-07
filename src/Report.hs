{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Report
  ( AnalyticsRow (..)
  , renderAnalyticsCsv
  , CurvePointSummary (..)
  , CurveJson (..)
  , renderCurveJson
  , parseCurveJson
  , renderCurveReportJson
  , Manifest (..)
  , renderManifest
  , compoundingText
  , dayCountText
  , interpolationText
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:))
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import qualified Data.Vector as V
import Numeric (showFFloat)

import Curve.Build (CurveReport (..))
import Data.ECB (SourceMeta (..))
import Domain

-- Analytics CSV ---------------------------------------------------------------

data AnalyticsRow = AnalyticsRow
  { arId :: !T.Text
  , arClean :: !Scientific
  , arDirty :: !Scientific
  , arAccrued :: !Scientific
  , arYtmCc :: !(Maybe Scientific)
  , arModDuration :: !Double
  , arMacaulayDuration :: !Double
  , arConvexity :: !Double
  , arDv01 :: !Scientific
  , arKrd2 :: !Scientific
  , arKrd5 :: !Scientific
  , arKrd10 :: !Scientific
  , arKrd30 :: !Scientific
  } deriving (Eq, Show)

analyticsHeader :: Csv.Header
analyticsHeader = V.fromList
  [ "id"
  , "clean"
  , "dirty"
  , "accrued"
  , "ytm_cc"
  , "mod_duration"
  , "macaulay_duration"
  , "convexity"
  , "dv01"
  , "krd_2"
  , "krd_5"
  , "krd_10"
  , "krd_30"
  ]

instance Csv.ToNamedRecord AnalyticsRow where
  toNamedRecord AnalyticsRow{..} = Csv.namedRecord
    [ "id" Csv..= arId
    , "clean" Csv..= arClean
    , "dirty" Csv..= arDirty
    , "accrued" Csv..= arAccrued
    , "ytm_cc" Csv..= arYtmCc
    , "mod_duration" Csv..= arModDuration
    , "macaulay_duration" Csv..= arMacaulayDuration
    , "convexity" Csv..= arConvexity
    , "dv01" Csv..= arDv01
    , "krd_2" Csv..= arKrd2
    , "krd_5" Csv..= arKrd5
    , "krd_10" Csv..= arKrd10
    , "krd_30" Csv..= arKrd30
    ]

instance Csv.DefaultOrdered AnalyticsRow where
  headerOrder _ = analyticsHeader

instance Csv.FromNamedRecord AnalyticsRow where
  parseNamedRecord m =
    AnalyticsRow
      <$> m Csv..: "id"
      <*> m Csv..: "clean"
      <*> m Csv..: "dirty"
      <*> m Csv..: "accrued"
      <*> m Csv..: "ytm_cc"
      <*> m Csv..: "mod_duration"
      <*> m Csv..: "macaulay_duration"
      <*> m Csv..: "convexity"
      <*> m Csv..: "dv01"
      <*> m Csv..: "krd_2"
      <*> m Csv..: "krd_5"
      <*> m Csv..: "krd_10"
      <*> m Csv..: "krd_30"

renderAnalyticsCsv :: [AnalyticsRow] -> BL.ByteString
renderAnalyticsCsv rows = Csv.encodeByName analyticsHeader rows

-- Curve JSON -----------------------------------------------------------------

data CurvePointSummary = CurvePointSummary
  { cpsTenorYears :: !Double
  , cpsZeroRate :: !Double
  , cpsDiscountFactor :: !Double
  } deriving (Eq, Show)

data CurveJson = CurveJson
  { cjAsOfDate :: !Day
  , cjConfig :: !CurveConfig
  , cjSource :: !SourceMeta
  , cjPoints :: ![CurvePointSummary]
  , cjReport :: !CurveReport
  } deriving (Eq, Show)

renderCurveJson :: CurveJson -> BL.ByteString
renderCurveJson = Aeson.encode

parseCurveJson :: BL.ByteString -> Either String CurveJson
parseCurveJson = Aeson.eitherDecode

renderCurveReportJson :: CurveJson -> BL.ByteString
renderCurveReportJson CurveJson{..} =
  Aeson.encode $ Aeson.object
    [ "as_of_date" .= dayText cjAsOfDate
    , "checks" .= curveReportValue cjReport
    ]

instance Aeson.ToJSON CurveJson where
  toJSON CurveJson{..} = Aeson.object
    [ "as_of_date" .= dayText cjAsOfDate
    , "interpolation" .= interpolationText (cfgInterpolation cjConfig)
    , "extrapolation" .= extrapolationText (cfgExtrapolation cjConfig)
    , "conventions" .= Aeson.object
        [ "compounding" .= compoundingText (cfgCompounding cjConfig)
        , "day_count" .= dayCountText (cfgDayCount cjConfig)
        ]
    , "source_meta" .= sourceMetaValue cjSource
    , "points" .= map Aeson.toJSON cjPoints
    , "checks" .= curveReportValue cjReport
    , "key_rate_tenors" .= map tenorToJSON (cfgKeyRateTenors cjConfig)
    ]

instance Aeson.FromJSON CurveJson where
  parseJSON = Aeson.withObject "CurveJson" $ \o -> do
    dateTxt <- o Aeson..: "as_of_date"
    asOf <- parseDay dateTxt
    interpTxt <- o Aeson..: "interpolation"
    interp <- parseInterpolation interpTxt
    extraTxt <- o Aeson..:? "extrapolation" Aeson..!= extrapolationText (cfgExtrapolation defaultCurveConfig)
    extra <- parseExtrapolation extraTxt
    conventions <- o Aeson..: "conventions"
    compTxt <- conventions Aeson..: "compounding"
    comp <- parseCompounding compTxt
    dcTxt <- conventions Aeson..: "day_count"
    dayCount <- parseDayCount dcTxt
    source <- o Aeson..: "source_meta" >>= parseSourceMeta
    points <- o Aeson..: "points"
    report <- o Aeson..: "checks" >>= parseCurveReport
    keyRateValues <- o Aeson..:? "key_rate_tenors" Aeson..!= []
    let config = CurveConfig
          { cfgCompounding = comp
          , cfgDayCount = dayCount
          , cfgInterpolation = interp
          , cfgExtrapolation = extra
          , cfgKeyRateTenors = map TenorYears keyRateValues
          }
    pure CurveJson
      { cjAsOfDate = asOf
      , cjConfig = config
      , cjSource = source
      , cjPoints = points
      , cjReport = report
      }

instance Aeson.ToJSON CurvePointSummary where
  toJSON CurvePointSummary{..} = Aeson.object
    [ "tenor_years" .= cpsTenorYears
    , "zero_rate_cc" .= cpsZeroRate
    , "df" .= cpsDiscountFactor
    ]

instance Aeson.FromJSON CurvePointSummary where
  parseJSON = Aeson.withObject "CurvePointSummary" $ \o ->
    CurvePointSummary
      <$> o .: "tenor_years"
      <*> o .: "zero_rate_cc"
      <*> o .: "df"

-- Manifest -------------------------------------------------------------------

data Manifest = Manifest
  { manAsOfDate :: !Day
  , manCurveHash :: !T.Text
  , manBondsHash :: !T.Text
  , manKrdKeys :: ![TenorYears]
  , manCompounding :: !Compounding
  , manDayCount :: !DayCountConvention
  , manInterpolation :: !InterpolationMethod
  , manGitCommit :: !(Maybe T.Text)
  , manTimestamp :: !T.Text
  } deriving (Eq, Show)

renderManifest :: Manifest -> T.Text
renderManifest Manifest{..} = T.unlines
  [ line "as_of_date" (dayText manAsOfDate)
  , line "curve_json_sha256" manCurveHash
  , line "bonds_csv_sha256" manBondsHash
  , line "krd_keys" (keysText manKrdKeys)
  , line "compounding" (compoundingText manCompounding)
  , line "day_count" (dayCountText manDayCount)
  , line "interp" (interpolationText manInterpolation)
  , line "git_commit" (fromMaybe "" manGitCommit)
  , line "timestamp" manTimestamp
  ]
  where
    line k v = k <> "=" <> v
    keysText = T.pack . intercalate "," . map formatTenor
    formatTenor (TenorYears x) =
      let rounded = fromInteger (round x) :: Double
       in if abs (x - rounded) < 1e-9
            then show (round x :: Integer)
            else showFFloat Nothing x ""

-- Helpers --------------------------------------------------------------------

dayText :: Day -> T.Text
dayText = T.pack . formatTime defaultTimeLocale "%Y-%m-%d"

parseDay :: T.Text -> Parser Day
parseDay txt =
  maybe (fail ("Invalid date: " <> T.unpack txt)) pure
    (parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack txt))

sourceMetaValue :: SourceMeta -> Aeson.Value
sourceMetaValue SourceMeta{..} = Aeson.object
  [ "provider" .= smProvider
  , "series_id" .= smSeriesId
  , "file_name" .= smFileName
  , "hash" .= smDigest
  ]

parseSourceMeta :: Aeson.Value -> Parser SourceMeta
parseSourceMeta = Aeson.withObject "SourceMeta" $ \o ->
  SourceMeta
    <$> o .: "provider"
    <*> o .: "series_id"
    <*> o .: "file_name"
    <*> o .: "hash"

curveReportValue :: CurveReport -> Aeson.Value
curveReportValue CurveReport{..} = Aeson.object
  [ "monotone_df" .= crMonotoneDf
  , "min_df" .= crMinDf
  , "max_df" .= crMaxDf
  , "neg_forward_share" .= crNegForwardShare
  ]

parseCurveReport :: Aeson.Value -> Parser CurveReport
parseCurveReport = Aeson.withObject "CurveReport" $ \o ->
  CurveReport
    <$> o .: "monotone_df"
    <*> o .: "min_df"
    <*> o .: "max_df"
    <*> o .: "neg_forward_share"

compoundingText :: Compounding -> T.Text
compoundingText Continuous = "CC"

dayCountText :: DayCountConvention -> T.Text
dayCountText Act365F = "ACT/365"

interpolationText :: InterpolationMethod -> T.Text
interpolationText LogDiscountLinear = "log_df_linear"

extrapolationText :: ExtrapolationPolicy -> T.Text
extrapolationText FlatForward = "flat_forward"
extrapolationText ClampToLast = "clamp"

parseCompounding :: T.Text -> Parser Compounding
parseCompounding "CC" = pure Continuous
parseCompounding other = fail ("Unsupported compounding: " <> T.unpack other)

parseDayCount :: T.Text -> Parser DayCountConvention
parseDayCount "ACT/365" = pure Act365F
parseDayCount other = fail ("Unsupported day_count: " <> T.unpack other)

parseInterpolation :: T.Text -> Parser InterpolationMethod
parseInterpolation "log_df_linear" = pure LogDiscountLinear
parseInterpolation other = fail ("Unsupported interpolation: " <> T.unpack other)

parseExtrapolation :: T.Text -> Parser ExtrapolationPolicy
parseExtrapolation "flat_forward" = pure FlatForward
parseExtrapolation "clamp" = pure ClampToLast
parseExtrapolation other = fail ("Unsupported extrapolation: " <> T.unpack other)

tenorToJSON :: TenorYears -> Aeson.Value
tenorToJSON (TenorYears x) = Aeson.toJSON x
