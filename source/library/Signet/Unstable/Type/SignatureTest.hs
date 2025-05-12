module Signet.Unstable.Type.SignatureTest where

import qualified Crypto.Error as Error
import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidAsymmetricSignature as InvalidAsymmetricSignature
import qualified Signet.Unstable.Exception.InvalidSignature as InvalidSignature
import qualified Signet.Unstable.Exception.InvalidSymmetricSignature as InvalidSymmetricSignature
import qualified Signet.Unstable.Type.AsymmetricSignature as AsymmetricSignature
import qualified Signet.Unstable.Type.Signature as Signature
import qualified Signet.Unstable.Type.SymmetricSignature as SymmetricSignature
import qualified Signet.Unstable.Type.Test as Test
import qualified Signet.Unstable.Type.UnknownSignature as UnknownSignature

spec :: (Monad tree) => Test.Test tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Type.Signature" $ do
  Test.describe test "parse" $ do
    Test.it test "fails with invalid asymmetric signature" $ do
      let byteString = Ascii.pack "invalid"
      let result = Signature.parse $ Ascii.pack "v1a," <> byteString
      Test.assertEq test result (Left (InvalidSignature.InvalidAsymmetricSignature $ InvalidAsymmetricSignature.MkInvalidAsymmetricSignature byteString))

    Test.it test "fails with invalid symmetric signature" $ do
      let byteString = Ascii.pack "invalid"
      let result = Signature.parse $ Ascii.pack "v1," <> byteString
      Test.assertEq test result (Left (InvalidSignature.InvalidSymmetricSignature $ InvalidSymmetricSignature.MkInvalidSymmetricSignature byteString))

    Test.it test "returns unknown signature with unrecognized prefix" $ do
      let byteString = Ascii.pack "unknown"
      let result = Signature.parse byteString
      Test.assertEq test result (Right (Left $ UnknownSignature.MkUnknownSignature byteString))

    Test.it test "succeeds with valid asymmetric signature" $ do
      let byteString = Ascii.pack "v1a,QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVotMDEyMzQ1Njc4OS1hYmNkZWZnaGlqa2xtbm9wcXJzdHV2cXh5eg=="
      signature <- fmap (Signature.Asymmetric . AsymmetricSignature.MkAsymmetricSignature) . Error.throwCryptoErrorIO . Ed25519.signature $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789-abcdefghijklmnopqrstuvqxyz"
      Test.assertEq test (Signature.parse byteString) (Right (Right signature))

    Test.it test "succeeds with valid symmetric signature" $ do
      let byteString = Ascii.pack "v1,47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
      let signature = Signature.Symmetric . SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      Test.assertEq test (Signature.parse byteString) (Right (Right signature))

  Test.describe test "render" $ do
    Test.it test "renders asymmetric signature correctly" $ do
      signature <- fmap (Signature.Asymmetric . AsymmetricSignature.MkAsymmetricSignature) . Error.throwCryptoErrorIO . Ed25519.signature $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789-abcdefghijklmnopqrstuvqxyz"
      Test.assertEq test (Signature.render signature) (Ascii.pack "v1a,QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVotMDEyMzQ1Njc4OS1hYmNkZWZnaGlqa2xtbm9wcXJzdHV2cXh5eg==")

    Test.it test "renders symmetric signature correctly" $ do
      let signature = Signature.Symmetric . SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      Test.assertEq test (Signature.render signature) (Ascii.pack "v1,47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=")
