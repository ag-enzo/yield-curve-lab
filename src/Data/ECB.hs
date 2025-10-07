{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.ECB
  ( CurvePointRaw (..)
  , CurvePoint (..)
  , SourceMeta (..)
  , loadCurveCsv
  , computeSourceMeta
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import Data.Hashable (hash)
import qualified Data.Text as T
import Data.Time (Day)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import System.FilePath (takeFileName)

import Domain

-- | Raw curve row as emitted by our tidy CSV.
data CurvePointRaw = CurvePointRaw
  { cprDate :: !Day
  , cprTenorYears :: !Double
  , cprZeroRate :: !Double
  } deriving (Eq, Show, Generic)

instance Csv.FromNamedRecord CurvePointRaw where
  parseNamedRecord m =
    do
      dateField <- m Csv..: "date"
      day <- parseDateField dateField
      tenor <- m Csv..: "tenor_years"
      rate <- m Csv..: "zero_rate_cc"
      pure CurvePointRaw
        { cprDate = day
        , cprTenorYears = tenor
        , cprZeroRate = rate
        }

-- | Validated curve point in domain types.
data CurvePoint = CurvePoint
  { cpDate :: !Day
  , cpTenor :: !TenorYears
  , cpZeroRate :: !RateCC
  } deriving (Eq, Show)

-- | Metadata recorded about the source curve file.
data SourceMeta = SourceMeta
  { smProvider :: !T.Text
  , smSeriesId :: !T.Text
  , smFileName :: !T.Text
  , smDigest :: !T.Text
  } deriving (Eq, Show)

-- | Load and validate an ECB CSV file.
loadCurveCsv :: FilePath -> IO (Either DomainError [CurvePoint])
loadCurveCsv fp = do
  csvData <- BL.readFile fp
  pure $ do
    (_, rows) <- firstCsvError (Csv.decodeByName csvData)
    points <- traverse toCurvePoint (V.toList rows)
    enforceAscendingTenors points
  where
    toCurvePoint :: CurvePointRaw -> Either DomainError CurvePoint
    toCurvePoint CurvePointRaw{..} = do
      tenor <- mkTenorYears cprTenorYears
      rate <- mkRateCC (cprZeroRate / 100.0)
      pure CurvePoint
        { cpDate = cprDate
        , cpTenor = tenor
        , cpZeroRate = rate
        }

-- | Compute simple provenance metadata (hash is best-effort for manifest).
computeSourceMeta :: FilePath -> BL.ByteString -> SourceMeta
computeSourceMeta fp payload =
  SourceMeta
    { smProvider = "ECB"
    , smSeriesId = "YC.B.U2.EUR.4F.G_N_A.SV_C_YM"
    , smFileName = T.pack (takeFileName fp)
    , smDigest = digestText
    }
  where
    digestText = T.pack (show (hash (BL.toStrict payload)))

-- Internal helpers -----------------------------------------------------------

enforceAscendingTenors :: [CurvePoint] -> Either DomainError [CurvePoint]
enforceAscendingTenors pts
  | strictlyAscending (fmap (unTenorYears . cpTenor) pts) = Right pts
  | otherwise = Left NonAscendingTenors

strictlyAscending :: Ord a => [a] -> Bool
strictlyAscending [] = True
strictlyAscending [_] = True
strictlyAscending (x:y:rest) = x < y && strictlyAscending (y : rest)

firstCsvError :: Either String a -> Either DomainError a
firstCsvError = either (Left . CsvParseError . T.pack) Right

parseDateField :: BS.ByteString -> Csv.Parser Day
parseDateField bs =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (BS.unpack bs) of
    Just day -> pure day
    Nothing -> fail "Invalid date format"
