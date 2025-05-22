module Signet.Unstable.Type.ToleranceTest where

import qualified Data.Time as Time
import qualified Heck as Test
import qualified Signet.Unstable.Exception.ToleranceException as ToleranceException
import qualified Signet.Unstable.Type.Timestamp as Timestamp
import qualified Signet.Unstable.Type.Tolerance as Tolerance

spec :: (Applicative io, Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Type.Tolerance" $ do
  Test.describe test "check" $ do
    Test.it test "succeeds when timestamp is within tolerance" $ do
      let tolerance = Tolerance.MkTolerance 1
      let utcTime = Time.UTCTime (Time.fromGregorian 2001 1 1) 60
      let timestamp = Timestamp.MkTimestamp utcTime
      let result = Tolerance.check tolerance utcTime timestamp
      Test.assertEq test result (Right ())

    Test.it test "fails when timestamp is too old" $ do
      let tolerance = Tolerance.MkTolerance 1
      let utcTime = Time.UTCTime (Time.fromGregorian 2001 1 1) 60
      let timestamp = Timestamp.MkTimestamp utcTime
      let now = Time.addUTCTime 2 utcTime
      let result = Tolerance.check tolerance now timestamp
      Test.assertEq test result (Left (ToleranceException.MkToleranceException timestamp))

    Test.it test "fails when timestamp is in the future" $ do
      let tolerance = Tolerance.MkTolerance 1
      let utcTime = Time.UTCTime (Time.fromGregorian 2001 1 1) 60
      let timestamp = Timestamp.MkTimestamp utcTime
      let now = Time.addUTCTime (-2) utcTime
      let result = Tolerance.check tolerance now timestamp
      Test.assertEq test result (Left (ToleranceException.MkToleranceException timestamp))
