module Signet.Unstable.Type.TimestampTest where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString.Char8 as Ascii
import qualified Data.Time as Time
import qualified Signet.Unstable.Exception.InvalidTimestamp as InvalidTimestamp
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Type.Test as Test
import qualified Signet.Unstable.Type.Timestamp as Timestamp

spec :: (Exception.MonadThrow io, Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Type.Timestamp" $ do
  Test.describe test "parse" $ do
    Test.it test "fails with invalid timestamp format" $ do
      let byteString = Ascii.pack "invalid-timestamp"
      let result = Timestamp.parse byteString
      Test.assertEq test result (Left (InvalidTimestamp.MkInvalidTimestamp byteString))

    Test.it test "succeeds with valid timestamp format" $ do
      let byteString = Ascii.pack "1617235200"
      let result = Timestamp.parse byteString
      timestamp <- Either.throw result
      let utcTime = Timestamp.unwrap timestamp
      let expectedTime = Time.UTCTime (Time.fromGregorian 2021 4 1) 0
      Test.assertEq test utcTime expectedTime

  Test.describe test "render" $ do
    Test.it test "returns the correct ByteString representation" $ do
      let utcTime = Time.UTCTime (Time.fromGregorian 2021 4 1) 0
      let timestamp = Timestamp.MkTimestamp utcTime
      let expected = Ascii.pack "1617235200"
      Test.assertEq test (Timestamp.render timestamp) expected
