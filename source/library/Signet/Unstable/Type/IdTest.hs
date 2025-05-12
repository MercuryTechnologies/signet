module Signet.Unstable.Type.IdTest where

import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidId as InvalidId
import qualified Signet.Unstable.Type.Id as Id
import qualified Signet.Unstable.Type.Test as Test

spec :: (Monad tree) => Test.Test tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Type.Id" $ do
  Test.describe test "parse" $ do
    Test.it test "fails with input containing separator" $ do
      let byteString = Ascii.pack "invalid.id"
      let result = Id.parse byteString
      Test.assertEq test result (Left $ InvalidId.MkInvalidId byteString)

    Test.it test "succeeds with valid input" $ do
      let byteString = Ascii.pack "valid-id"
      let result = Id.parse byteString
      Test.assertEq test result (Right $ Id.MkId byteString)

  Test.describe test "render" $ do
    Test.it test "returns the original ByteString" $ do
      let byteString = Ascii.pack "valid-id"
      let id_ = Id.MkId byteString
      Test.assertEq test (Id.render id_) byteString
