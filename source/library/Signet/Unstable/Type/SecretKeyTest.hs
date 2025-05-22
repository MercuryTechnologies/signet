module Signet.Unstable.Type.SecretKeyTest where

import qualified Control.Monad.Catch as Exception
import qualified Crypto.Error as Error
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString.Char8 as Ascii
import qualified Heck as Test
import qualified Signet.Unstable.Exception.InvalidSecretKey as InvalidSecretKey
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Type.SecretKey as SecretKey

spec :: (Exception.MonadThrow io, Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Type.SecretKey" $ do
  Test.describe test "parse" $ do
    Test.it test "fails with invalid prefix" $ do
      let byteString = Ascii.pack "invalid"
      let result = SecretKey.parse byteString
      Test.assertEq test result (Left (InvalidSecretKey.MkInvalidSecretKey byteString))

    Test.it test "fails with invalid input" $ do
      let byteString = Ascii.pack "whsk_invalid"
      let result = SecretKey.parse byteString
      Test.assertEq test result (Left (InvalidSecretKey.MkInvalidSecretKey byteString))

    Test.it test "succeeds with valid input" $ do
      let result = SecretKey.parse $ Ascii.pack "whsk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU="
      secretKey <- Either.throw . fmap SecretKey.MkSecretKey . Error.eitherCryptoError . Ed25519.secretKey $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ012345"
      Test.assertEq test result (Right secretKey)

  Test.describe test "render" $ do
    Test.it test "works" $ do
      secretKey <- Either.throw . fmap SecretKey.MkSecretKey . Error.eitherCryptoError . Ed25519.secretKey $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ012345"
      Test.assertEq test (SecretKey.render secretKey) (Ascii.pack "whsk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU=")
