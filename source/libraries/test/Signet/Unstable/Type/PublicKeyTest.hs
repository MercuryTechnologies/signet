module Signet.Unstable.Type.PublicKeyTest where

import qualified Crypto.Error as Error
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidPublicKey as InvalidPublicKey
import qualified Signet.Unstable.Extra.Tasty as Tasty
import qualified Signet.Unstable.Type.PublicKey as PublicKey
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Type.PublicKey" $ do
  Tasty.describe "parse" $ do
    Tasty.it "fails with invalid prefix" $ do
      let byteString = Ascii.pack "invalid"
      let result = PublicKey.parse byteString
      result @?= Left (InvalidPublicKey.MkInvalidPublicKey byteString)

    Tasty.it "fails with invalid input" $ do
      let byteString = Ascii.pack "whpk_invalid"
      let result = PublicKey.parse byteString
      result @?= Left (InvalidPublicKey.MkInvalidPublicKey byteString)

    Tasty.it "succeeds with valid input" $ do
      let result = PublicKey.parse $ Ascii.pack "whpk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU="
      publicKey <- fmap PublicKey.MkPublicKey . Error.throwCryptoErrorIO . Ed25519.publicKey $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ012345"
      result @?= Right publicKey

  Tasty.describe "render" $ do
    Tasty.it "works" $ do
      publicKey <- fmap PublicKey.MkPublicKey . Error.throwCryptoErrorIO . Ed25519.publicKey $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ012345"
      PublicKey.render publicKey @?= Ascii.pack "whpk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU="
