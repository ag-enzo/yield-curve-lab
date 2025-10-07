import Test.Hspec

import qualified BondSpec
import qualified CurveBuildSpec
import qualified CurveSpec
import qualified GoldenSpec
import qualified ReportSpec
import qualified RiskSpec

main :: IO ()
main = hspec $ do
  CurveSpec.spec
  CurveBuildSpec.spec
  BondSpec.spec
  ReportSpec.spec
  RiskSpec.spec
  GoldenSpec.spec
