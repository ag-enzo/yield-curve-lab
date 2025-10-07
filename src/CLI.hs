{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module CLI (runCLI) where

import Control.Monad (when)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.Hashable (hash)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import qualified Data.Csv as Csv
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import qualified Data.Vector as V
import Options.Applicative
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist)
import System.Exit (die)
import System.FilePath (takeDirectory, (</>))
import Text.Read (readMaybe)

import Curve.Build
import Data.ECB (CurvePoint (..), computeSourceMeta, loadCurveCsv)
import Domain
import Pricing.Bond
import Report
import Risk.Measures

-- CLI options ----------------------------------------------------------------

data Command
  = CmdFetch FetchOptions
  | CmdBuild BuildOptions
  | CmdPrice PriceOptions

data FetchOptions = FetchOptions
  { foDate :: !String
  , foSource :: !String
  , foOutPath :: !FilePath
  }

data BuildOptions = BuildOptions
  { boCurveCsv :: !FilePath
  , boCurveJson :: !FilePath
  , boReportJson :: !FilePath
  }

data PriceOptions = PriceOptions
  { poCurveJson :: !FilePath
  , poBondsCsv :: !FilePath
  , poAnalyticsCsv :: !FilePath
  , poManifestPath :: !(Maybe FilePath)
  , poKeyRateTenors :: ![TenorYears]
  }

runCLI :: IO ()
runCLI = execParser opts >>= runCommand
  where
    opts = info (commandParser <**> helper)
      ( fullDesc
     <> progDesc "YieldCurveLab CLI"
     <> header "ycl - yield curve analytics toolkit" )

commandParser :: Parser Command
commandParser = hsubparser
  ( command "fetch" (info (CmdFetch <$> fetchParser) (progDesc "Fetch curve data (offline stub)"))
 <> command "build" (info (CmdBuild <$> buildParser) (progDesc "Build discount curve from CSV"))
 <> command "price" (info (CmdPrice <$> priceParser) (progDesc "Price bonds and compute risk"))
  )

fetchParser :: Parser FetchOptions
fetchParser = FetchOptions
  <$> strOption (long "date" <> metavar "YYYY-MM-DD" <> help "Curve date" <> value "2025-10-02")
  <*> strOption (long "source" <> metavar "SOURCE" <> help "Data source (e.g. ecb)" <> value "ecb")
  <*> strOption (long "out" <> metavar "FILE" <> help "Output CSV path")

buildParser :: Parser BuildOptions
buildParser = BuildOptions
  <$> strOption (long "curve" <> metavar "CSV" <> help "Curve CSV input")
  <*> strOption (long "out" <> metavar "CURVE_JSON" <> help "Curve JSON output")
  <*> strOption (long "report" <> metavar "REPORT_JSON" <> help "Curve report JSON output")

priceParser :: Parser PriceOptions
priceParser = PriceOptions
  <$> strOption (long "curve" <> metavar "CURVE_JSON" <> help "Curve JSON input")
  <*> strOption (long "bonds" <> metavar "BONDS_CSV" <> help "Bonds CSV input")
  <*> strOption (long "out" <> metavar "ANALYTICS_CSV" <> help "Analytics CSV output")
  <*> optional (strOption (long "manifest" <> metavar "MANIFEST" <> help "Manifest output"))
  <*> option (maybeReader parseKeys) (long "krd" <> metavar "LIST" <> help "Comma-separated key-rate tenors" <> value (map TenorYears [2,5,10,30]))
  where
    parseKeys txt = traverse (fmap TenorYears . readMaybeStrip) (splitComma txt)

-- Execution ------------------------------------------------------------------

runCommand :: Command -> IO ()
runCommand (CmdFetch fo) = runFetch fo
runCommand (CmdBuild bo) = runBuild bo
runCommand (CmdPrice po) = runPrice po

runFetch :: FetchOptions -> IO ()
runFetch FetchOptions{..} = do
  let candidate = "data/ecb_sample_" <> foDate <> ".csv"
  exists <- doesFileExist candidate
  let sourcePath = if exists then candidate else "data/ecb_sample_2025-10-02.csv"
  createDirectoryIfMissing True (takeDirectory foOutPath)
  copyFile sourcePath foOutPath
  putStrLn $ "Copied sample curve from " <> sourcePath <> " to " <> foOutPath

runBuild :: BuildOptions -> IO ()
runBuild BuildOptions{..} = do
  csvBytes <- BL.readFile boCurveCsv
  curvePointsResult <- loadCurveCsv boCurveCsv
  points <- either (die . displayDomainError) pure curvePointsResult
  let config = defaultCurveConfig
  case buildCurve config points of
    Left err -> die (displayDomainError err)
    Right (curve, report) -> do
      let meta = computeSourceMeta boCurveCsv csvBytes
          curveJson = CurveJson
            { cjAsOfDate = dcAsOf curve
            , cjConfig = config
            , cjSource = meta
            , cjPoints = curvePointsSummaries curve
            , cjReport = report
            }
      writeLazy boCurveJson (renderCurveJson curveJson)
      writeLazy boReportJson (renderCurveReportJson curveJson)
      putStrLn $ "Curve written to " <> boCurveJson
      putStrLn $ "Report written to " <> boReportJson

runPrice :: PriceOptions -> IO ()
runPrice PriceOptions{..} = do
  curveBytes <- BL.readFile poCurveJson
  curveJson <- either die pure (parseCurveJson curveBytes)
  let points = map (summaryToPoint (cjAsOfDate curveJson)) (cjPoints curveJson)
      config = cjConfig curveJson
  case buildCurve config points of
    Left err -> die (displayDomainError err)
    Right (curve, _) -> do
      bondsBytes <- BL.readFile poBondsCsv
      bondRecords <- either die pure (decodeBonds bondsBytes)
      bonds <- traverse toBond (V.toList bondRecords)
      let requiredKeys = map TenorYears [2,5,10,30]
      when (any (`notElem` poKeyRateTenors) requiredKeys) $
        putStrLn "Warning: analytics columns are fixed to 2/5/10/30 KRDs; missing keys will be reported as 0."
      analyticsRows <- traverse (bondAnalytics curve poKeyRateTenors) bonds
      writeLazy poAnalyticsCsv (renderAnalyticsCsv analyticsRows)
      let manifestPath = fromMaybe (defaultManifestPath poAnalyticsCsv) poManifestPath
      curveHash <- fileHash poCurveJson
      bondsHash <- fileHash poBondsCsv
      timestamp <- currentTimestamp
      let manifest = Manifest
            { manAsOfDate = cjAsOfDate curveJson
            , manCurveHash = curveHash
            , manBondsHash = bondsHash
            , manKrdKeys = poKeyRateTenors
            , manCompounding = cfgCompounding config
            , manDayCount = cfgDayCount config
            , manInterpolation = cfgInterpolation config
            , manGitCommit = Nothing
            , manTimestamp = T.pack timestamp
            }
      writeText manifestPath (renderManifest manifest)
      putStrLn $ "Analytics written to " <> poAnalyticsCsv
      putStrLn $ "Manifest written to " <> manifestPath

-- Helpers --------------------------------------------------------------------

curvePointsSummaries :: DiscountCurve -> [CurvePointSummary]
curvePointsSummaries DiscountCurve{..} =
  [ CurvePointSummary
      (pillarTenorValue p)
      (unRateCC (pillarRate p))
      (unDiscountFactor (pillarDiscount p))
  | p <- NE.toList dcPillars
  ]

summaryToPoint :: Day -> CurvePointSummary -> CurvePoint
summaryToPoint day CurvePointSummary{..} =
  CurvePoint
    { cpDate = day
    , cpTenor = TenorYears cpsTenorYears
    , cpZeroRate = RateCC cpsZeroRate
    }

bondAnalytics :: DiscountCurve -> [TenorYears] -> Bond -> IO AnalyticsRow
bondAnalytics curve krdKeys bond = do
  dirty <- either (die . displayDomainError) pure (dirtyPrice curve bond)
  accrued <- either (die . displayDomainError) pure (accruedInterest bond)
  clean <- either (die . displayDomainError) pure (cleanPrice curve bond)
  macDur <- either (die . displayDomainError) pure (macaulayDuration curve bond)
  modDur <- either (die . displayDomainError) pure (modifiedDuration curve bond)
  conv <- either (die . displayDomainError) pure (convexity curve bond)
  dv <- either (die . displayDomainError) pure (dv01Parallel curve bond)
  krds <- either (die . displayDomainError) pure (keyRateDV01 curve bond krdKeys)
  let krdMap = Map.fromList krds
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

decodeBonds :: BL.ByteString -> Either String (V.Vector BondCsvRow)
decodeBonds bytes = do
  (_, vec) <- Csv.decodeByName bytes
  pure vec

data BondCsvRow = BondCsvRow
  { bcrId :: !T.Text
  , bcrSettlement :: !Day
  , bcrMaturity :: !Day
  , bcrCoupon :: !Double
  , bcrNotional :: !Scientific
  , bcrFrequency :: !Int
  }

instance Csv.FromNamedRecord BondCsvRow where
  parseNamedRecord m = do
    bcrId <- m Csv..: "id"
    settleTxt <- m Csv..: "settle_date"
    bcrSettlement <- parseDayField "settle_date" settleTxt
    maturityTxt <- m Csv..: "maturity"
    bcrMaturity <- parseDayField "maturity" maturityTxt
    bcrCoupon <- m Csv..: "coupon_rate"
    bcrNotional <- m Csv..: "notional"
    bcrFrequency <- m Csv..: "frequency"
    pure BondCsvRow{..}

parseDayField :: BS.ByteString -> BS.ByteString -> Csv.Parser Day
parseDayField fieldName bs =
  case parseTimeM True defaultTimeLocale "%Y-%m-%d" (BS.unpack bs) of
    Just day -> pure day
    Nothing -> fail ("Invalid date in field " <> BS.unpack fieldName)

toBond :: BondCsvRow -> IO Bond
toBond BondCsvRow{..} = do
  when (bcrFrequency /= 1) (die "Only annual coupons (frequency=1) supported in MVP")
  pure Bond
    { bondId = T.unpack bcrId
    , bondSettlement = bcrSettlement
    , bondMaturity = bcrMaturity
    , bondCouponRate = RateCC bcrCoupon
    , bondNotional = Money bcrNotional
    , bondFrequency = bcrFrequency
    }

fileHash :: FilePath -> IO T.Text
fileHash path = do
  bytes <- BL.readFile path
  pure . T.pack . show $ hash (BL.toStrict bytes)

currentTimestamp :: IO String
currentTimestamp = do
  now <- getCurrentTime
  pure (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)

writeLazy :: FilePath -> BL.ByteString -> IO ()
writeLazy path bytes = do
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path bytes

writeText :: FilePath -> T.Text -> IO ()
writeText path txt = do
  createDirectoryIfMissing True (takeDirectory path)
  TIO.writeFile path txt

defaultManifestPath :: FilePath -> FilePath
defaultManifestPath analyticsPath = takeDirectory analyticsPath </> "manifest.txt"

splitComma :: String -> [String]
splitComma = filter (not . null) . map trim . splitWords
  where
    splitWords = words . map replaceComma
    replaceComma c = if c == ',' then ' ' else c
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

readMaybeStrip :: String -> Maybe Double
readMaybeStrip = readMaybe . trim
  where trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

displayDomainError :: DomainError -> String
displayDomainError = show
