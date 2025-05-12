module Signet.Unstable.Type.SecretTest where

import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidSecret as InvalidSecret
import qualified Signet.Unstable.Extra.Tasty as Tasty
import qualified Signet.Unstable.Type.Secret as Secret
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Type.Secret" $ do
  Tasty.describe "parse" $ do
    Tasty.it "fails with invalid prefix" $ do
      let byteString = Ascii.pack "invalid"
      let result = Secret.parse byteString
      result @?= Left (InvalidSecret.MkInvalidSecret byteString)

    Tasty.it "fails with invalid input" $ do
      let byteString = Ascii.pack "whsec_invalid"
      let result = Secret.parse byteString
      result @?= Left (InvalidSecret.MkInvalidSecret byteString)

    Tasty.it "succeeds with valid input" $ do
      let result = Secret.parse $ Ascii.pack "whsec_MDEyMzQ1Njc4OQ=="
      let secret = Secret.MkSecret . ByteArray.convert $ Ascii.pack "0123456789"
      result @?= Right secret

  Tasty.describe "render" $ do
    Tasty.it "works" $ do
      let secret = Secret.MkSecret . ByteArray.convert $ Ascii.pack "0123456789"
      Secret.render secret @?= Ascii.pack "whsec_MDEyMzQ1Njc4OQ=="
