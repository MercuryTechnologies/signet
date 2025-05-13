module Signet.Unstable.Type.SecretTest where

import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidSecret as InvalidSecret
import qualified Signet.Unstable.Type.Secret as Secret
import qualified Signet.Unstable.Type.Test as Test

spec :: (Applicative io, Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Type.Secret" $ do
  Test.describe test "parse" $ do
    Test.it test "fails with invalid prefix" $ do
      let byteString = Ascii.pack "invalid"
      let result = Secret.parse byteString
      Test.assertEq test result (Left (InvalidSecret.MkInvalidSecret byteString))

    Test.it test "fails with invalid input" $ do
      let byteString = Ascii.pack "whsec_invalid"
      let result = Secret.parse byteString
      Test.assertEq test result (Left (InvalidSecret.MkInvalidSecret byteString))

    Test.it test "succeeds with valid input" $ do
      let result = Secret.parse $ Ascii.pack "whsec_MDEyMzQ1Njc4OQ=="
      let secret = Secret.MkSecret . ByteArray.convert $ Ascii.pack "0123456789"
      Test.assertEq test result (Right secret)

  Test.describe test "render" $ do
    Test.it test "works" $ do
      let secret = Secret.MkSecret . ByteArray.convert $ Ascii.pack "0123456789"
      Test.assertEq test (Secret.render secret) (Ascii.pack "whsec_MDEyMzQ1Njc4OQ==")
