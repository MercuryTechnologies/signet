module Signet.Unstable.Type.SecretKeyTest where

import qualified Crypto.Error as Error
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidSecretKey as InvalidSecretKey
import qualified Signet.Unstable.Extra.Tasty as Tasty
import qualified Signet.Unstable.Type.SecretKey as SecretKey
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Type.SecretKey" $ do
  Tasty.describe "parse" $ do
    Tasty.it "fails with invalid prefix" $ do
      let byteString = Ascii.pack "invalid"
      let result = SecretKey.parse byteString
      result @?= Left (InvalidSecretKey.MkInvalidSecretKey byteString)

    Tasty.it "fails with invalid input" $ do
      let byteString = Ascii.pack "whsk_invalid"
      let result = SecretKey.parse byteString
      result @?= Left (InvalidSecretKey.MkInvalidSecretKey byteString)

    Tasty.it "succeeds with valid input" $ do
      let result = SecretKey.parse $ Ascii.pack "whsk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU="
      secretKey <- fmap SecretKey.MkSecretKey . Error.throwCryptoErrorIO . Ed25519.secretKey $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ012345"
      result @?= Right secretKey

  Tasty.describe "render" $ do
    Tasty.it "works" $ do
      secretKey <- fmap SecretKey.MkSecretKey . Error.throwCryptoErrorIO . Ed25519.secretKey $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ012345"
      SecretKey.render secretKey @?= Ascii.pack "whsk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU="
