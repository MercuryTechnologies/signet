module Signet.Unstable.Type.PublicKeyTest where

import qualified Crypto.Error as Error
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidPublicKey as InvalidPublicKey
import qualified Signet.Unstable.Type.PublicKey as PublicKey
import qualified Signet.Unstable.Type.Test as Test

spec :: (Monad tree) => Test.Test tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Type.PublicKey" $ do
  Test.describe test "parse" $ do
    Test.it test "fails with invalid prefix" $ do
      let byteString = Ascii.pack "invalid"
      let result = PublicKey.parse byteString
      Test.assertEq test result (Left (InvalidPublicKey.MkInvalidPublicKey byteString))

    Test.it test "fails with invalid input" $ do
      let byteString = Ascii.pack "whpk_invalid"
      let result = PublicKey.parse byteString
      Test.assertEq test result (Left (InvalidPublicKey.MkInvalidPublicKey byteString))

    Test.it test "succeeds with valid input" $ do
      let result = PublicKey.parse $ Ascii.pack "whpk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU="
      publicKey <- fmap PublicKey.MkPublicKey . Error.throwCryptoErrorIO . Ed25519.publicKey $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ012345"
      Test.assertEq test result (Right publicKey)

  Test.describe test "render" $ do
    Test.it test "works" $ do
      publicKey <- fmap PublicKey.MkPublicKey . Error.throwCryptoErrorIO . Ed25519.publicKey $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ012345"
      Test.assertEq test (PublicKey.render publicKey) (Ascii.pack "whpk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU=")
