module ReportSpec (spec) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS
import qualified Data.Csv as Csv
import Data.Either (isRight)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Either (isRight)
import Data.Scientific (fromFloatDigits)
import Data.Time (fromGregorian)
import Test.Hspec

import Curve.Build (CurveReport (..))
import Data.ECB (SourceMeta (..))
import Domain
import Report

spec :: Spec
spec = describe "Report" $ do
  it "renders analytics CSV with expected header" $ do
    let row = AnalyticsRow
          { arId = T.pack "BOND-1"
          , arClean = fromFloatDigits (99.12 :: Double)
          , arDirty = fromFloatDigits (100.00 :: Double)
          , arAccrued = fromFloatDigits (0.88 :: Double)
          , arYtmCc = Just (fromFloatDigits (0.021 :: Double))
          , arModDuration = 4.2
          , arMacaulayDuration = 4.25
          , arConvexity = 18.0
          , arDv01 = fromFloatDigits (540.0 :: Double)
          , arKrd2 = fromFloatDigits (120.0 :: Double)
          , arKrd5 = fromFloatDigits (200.0 :: Double)
          , arKrd10 = fromFloatDigits (150.0 :: Double)
          , arKrd30 = fromFloatDigits (70.0 :: Double)
          }
        csvBytes = renderAnalyticsCsv [row]
        decoded = Csv.decodeByName csvBytes :: Either String (Csv.Header, V.Vector AnalyticsRow)
    decoded `shouldSatisfy` isRight
    case decoded of
      Left err -> expectationFailure err
      Right (hdr, vec) -> do
        hdr `shouldBe` V.fromList (map BS.pack
          [ "id","clean","dirty","accrued","ytm_cc","mod_duration"
          , "macaulay_duration","convexity","dv01","krd_2","krd_5","krd_10","krd_30"
          ])
        V.toList vec `shouldBe` [row]

  it "round-trips curve JSON" $ do
    let curveJson = CurveJson
          { cjAsOfDate = fromGregorian 2025 10 2
          , cjConfig = defaultCurveConfig
          , cjSource = SourceMeta
              { smProvider = T.pack "ECB"
              , smSeriesId = T.pack "SERIES"
              , smFileName = T.pack "file.csv"
              , smDigest = T.pack "hash123"
              }
          , cjPoints = [CurvePointSummary 0.5 0.02 (exp (-0.02 * 0.5))]
          , cjReport = CurveReport True 0.9 1.0 0.0
          }
        bytes = renderCurveJson curveJson
    parseCurveJson bytes `shouldBe` Right curveJson

  it "renders curve report JSON with checks" $ do
    let curveJson = CurveJson
          { cjAsOfDate = fromGregorian 2025 10 2
          , cjConfig = defaultCurveConfig
          , cjSource = SourceMeta (T.pack "ECB") (T.pack "SERIES") (T.pack "file.csv") (T.pack "hash123")
          , cjPoints = []
          , cjReport = CurveReport True 0.9 1.0 0.0
          }
        obj = Aeson.decode (renderCurveReportJson curveJson) :: Maybe Aeson.Object
    case obj of
      Nothing -> expectationFailure "Failed to decode curve report JSON"
      Just m -> do
        KM.member (Key.fromString "as_of_date") m `shouldBe` True
        case KM.lookup (Key.fromString "checks") m of
          Just (Aeson.Object checks) -> do
            mapM_ (\k -> KM.member (Key.fromString k) checks `shouldBe` True)
              ["monotone_df","min_df","max_df","neg_forward_share"]
          _ -> expectationFailure "checks field missing"

  it "renders manifest text lines" $ do
    let manifest = Manifest
          { manAsOfDate = fromGregorian 2025 10 2
          , manCurveHash = T.pack "curvehash"
          , manBondsHash = T.pack "bondshash"
          , manKrdKeys = map TenorYears [2,5,10,30]
          , manCompounding = Continuous
          , manDayCount = Act365F
          , manInterpolation = LogDiscountLinear
          , manGitCommit = Just (T.pack "abc123")
          , manTimestamp = T.pack "2025-10-02T12:00:00Z"
          }
        linesOut = T.lines (renderManifest manifest)
    mapM_ (\needle -> linesOut `shouldSatisfy` elem needle)
      [ T.pack "as_of_date=2025-10-02"
      , T.pack "git_commit=abc123"
      , T.pack "krd_keys=2,5,10,30"
      ]
