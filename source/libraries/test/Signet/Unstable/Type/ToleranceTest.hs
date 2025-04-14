module Signet.Unstable.Type.ToleranceTest where

import qualified Data.Time as Time
import qualified Signet.Unstable.Exception.ToleranceException as ToleranceException
import qualified Signet.Unstable.Extra.Tasty as Tasty
import qualified Signet.Unstable.Type.Timestamp as Timestamp
import qualified Signet.Unstable.Type.Tolerance as Tolerance
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Type.Tolerance" $ do
  Tasty.describe "check" $ do
    Tasty.it "succeeds when timestamp is within tolerance" $ do
      let tolerance = Tolerance.MkTolerance 1
      let utcTime = Time.UTCTime (Time.fromGregorian 2001 1 1) 60
      let timestamp = Timestamp.MkTimestamp utcTime
      let result = Tolerance.check tolerance utcTime timestamp
      result @?= Right ()

    Tasty.it "fails when timestamp is too old" $ do
      let tolerance = Tolerance.MkTolerance 1
      let utcTime = Time.UTCTime (Time.fromGregorian 2001 1 1) 60
      let timestamp = Timestamp.MkTimestamp utcTime
      let now = Time.addUTCTime 2 utcTime
      let result = Tolerance.check tolerance now timestamp
      result @?= Left (ToleranceException.MkToleranceException timestamp)

    Tasty.it "fails when timestamp is in the future" $ do
      let tolerance = Tolerance.MkTolerance 1
      let utcTime = Time.UTCTime (Time.fromGregorian 2001 1 1) 60
      let timestamp = Timestamp.MkTimestamp utcTime
      let now = Time.addUTCTime (-2) utcTime
      let result = Tolerance.check tolerance now timestamp
      result @?= Left (ToleranceException.MkToleranceException timestamp)
