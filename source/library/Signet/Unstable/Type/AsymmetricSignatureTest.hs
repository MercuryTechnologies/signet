module Signet.Unstable.Type.AsymmetricSignatureTest where

import qualified Control.Monad.Catch as Exception
import qualified Crypto.Error as Error
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidAsymmetricSignature as InvalidAsymmetricSignature
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Type.AsymmetricSignature as AsymmetricSignature
import qualified Signet.Unstable.Type.Test as Test

spec :: (Exception.MonadThrow io, Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Type.AsymmetricSignature" $ do
  Test.describe test "parse" $ do
    Test.it test "fails with invalid input" $ do
      let byteString = Ascii.pack "invalid"
      let result = AsymmetricSignature.parse byteString
      Test.assertEq test result (Left $ InvalidAsymmetricSignature.MkInvalidAsymmetricSignature byteString)

    Test.it test "succeeds with valid input" $ do
      let result = AsymmetricSignature.parse $ Ascii.pack "QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVotMDEyMzQ1Njc4OS1hYmNkZWZnaGlqa2xtbm9wcXJzdHV2cXh5eg=="
      signature <- Either.throw . Error.eitherCryptoError . Ed25519.signature $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789-abcdefghijklmnopqrstuvqxyz"
      Test.assertEq test result (Right $ AsymmetricSignature.MkAsymmetricSignature signature)

  Test.describe test "render" $ do
    Test.it test "works" $ do
      asymmetricSignature <- Either.throw . fmap AsymmetricSignature.MkAsymmetricSignature . Error.eitherCryptoError . Ed25519.signature $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789-abcdefghijklmnopqrstuvqxyz"
      Test.assertEq test (AsymmetricSignature.render asymmetricSignature) (Ascii.pack "QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVotMDEyMzQ1Njc4OS1hYmNkZWZnaGlqa2xtbm9wcXJzdHV2cXh5eg==")
