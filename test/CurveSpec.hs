module CurveSpec (spec) where

import qualified Data.ByteString.Char8 as BS
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec

import Data.ECB (loadCurveCsv)
import Domain (DomainError (..))

spec :: Spec
spec = describe "Data.ECB.loadCurveCsv" $ do
  it "rejects unsorted tenors" $ do
    let csvContent = BS.unlines
          [ BS.pack "date,tenor_years,zero_rate_cc"
          , BS.pack "2025-10-02,1.00,0.0200"
          , BS.pack "2025-10-02,0.50,0.0210"
          ]
    withTempCsv csvContent $ \fp -> do
      result <- loadCurveCsv fp
      result `shouldBe` Left NonAscendingTenors

  it "rejects invalid rates" $ do
    let csvContent = BS.unlines
          [ BS.pack "date,tenor_years,zero_rate_cc"
          , BS.pack "2025-10-02,0.25,NaN"
          , BS.pack "2025-10-02,0.50,0.0200"
          ]
    withTempCsv csvContent $ \fp -> do
      result <- loadCurveCsv fp
      case result of
        Left (CsvParseError _) -> pure ()
        _ -> expectationFailure "expected a CSV parse error"

withTempCsv :: BS.ByteString -> (FilePath -> IO a) -> IO a
withTempCsv contents action =
  withSystemTempFile "ecb.csv" $ \fp handle -> do
    BS.hPut handle contents
    hClose handle
    action fp
