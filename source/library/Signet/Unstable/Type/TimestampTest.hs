module Signet.Unstable.Type.TimestampTest where

import qualified Data.ByteString.Char8 as Ascii
import qualified Data.Time as Time
import qualified Signet.Unstable.Exception.InvalidTimestamp as InvalidTimestamp
import qualified Signet.Unstable.Extra.Tasty as Tasty
import qualified Signet.Unstable.Type.Timestamp as Timestamp
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Type.Timestamp" $ do
  Tasty.describe "parse" $ do
    Tasty.it "fails with invalid timestamp format" $ do
      let byteString = Ascii.pack "invalid-timestamp"
      let result = Timestamp.parse byteString
      result @?= Left (InvalidTimestamp.MkInvalidTimestamp byteString)

    Tasty.it "succeeds with valid timestamp format" $ do
      let byteString = Ascii.pack "1617235200"
      let result = Timestamp.parse byteString
      case result of
        Right timestamp -> do
          let utcTime = Timestamp.unwrap timestamp
          let expectedTime = Time.UTCTime (Time.fromGregorian 2021 4 1) 0
          utcTime @?= expectedTime
        Left _ -> fail "Expected Right but got Left"

  Tasty.describe "render" $ do
    Tasty.it "returns the correct ByteString representation" $ do
      let utcTime = Time.UTCTime (Time.fromGregorian 2021 4 1) 0
      let timestamp = Timestamp.MkTimestamp utcTime
      let expected = Ascii.pack "1617235200"
      Timestamp.render timestamp @?= expected
