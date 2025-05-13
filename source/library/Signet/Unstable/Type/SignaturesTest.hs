module Signet.Unstable.Type.SignaturesTest where

import qualified Control.Monad.Catch as Exception
import qualified Crypto.Error as Error
import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidAsymmetricSignature as InvalidAsymmetricSignature
import qualified Signet.Unstable.Exception.InvalidSignature as InvalidSignature
import qualified Signet.Unstable.Exception.InvalidSymmetricSignature as InvalidSymmetricSignature
import qualified Signet.Unstable.Exception.UnknownSignature as UnknownSignature
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Type.AsymmetricSignature as AsymmetricSignature
import qualified Signet.Unstable.Type.Signature as Signature
import qualified Signet.Unstable.Type.Signatures as Signatures
import qualified Signet.Unstable.Type.SymmetricSignature as SymmetricSignature
import qualified Signet.Unstable.Type.Test as Test

spec :: (Exception.MonadThrow io, Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Type.Signatures" $ do
  Test.describe test "parse" $ do
    Test.it test "succeeds with no signatures" $ do
      Test.assertEq test (Signatures.parse ByteString.empty) (Right ([], Signatures.MkSignatures []))

    Test.it test "succeeds with one signature" $ do
      let signature = Signature.Symmetric . SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      let byteString = Ascii.pack "v1,47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
      Test.assertEq test (Signatures.parse byteString) (Right ([], Signatures.MkSignatures [signature]))

    Test.it test "succeeds with many signatures" $ do
      let symmetricSignature = SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      asymmetricSignature <- Either.throw . fmap AsymmetricSignature.MkAsymmetricSignature . Error.eitherCryptoError . Ed25519.signature $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789-abcdefghijklmnopqrstuvqxyz"
      let byteString = Ascii.pack "v1,47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU= v1a,QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVotMDEyMzQ1Njc4OS1hYmNkZWZnaGlqa2xtbm9wcXJzdHV2cXh5eg=="
      let signatures =
            Signatures.MkSignatures
              [ Signature.Symmetric symmetricSignature,
                Signature.Asymmetric asymmetricSignature
              ]
      Test.assertEq test (Signatures.parse byteString) (Right ([], signatures))

    Test.it test "fails with an invalid symmetric signature" $ do
      let x = Ascii.pack "invalid"
      let byteString = Ascii.pack "v1," <> x
      Test.assertEq test (Signatures.parse byteString) (Left (InvalidSignature.InvalidSymmetricSignature $ InvalidSymmetricSignature.MkInvalidSymmetricSignature x))

    Test.it test "fails with an invalid asymmetric signature" $ do
      let x = Ascii.pack "invalid"
      let byteString = Ascii.pack "v1a," <> x
      Test.assertEq test (Signatures.parse byteString) (Left (InvalidSignature.InvalidAsymmetricSignature $ InvalidAsymmetricSignature.MkInvalidAsymmetricSignature x))

    Test.it test "succeeds with an unknown signature" $ do
      let byteString = Ascii.pack "unknown"
      Test.assertEq test (Signatures.parse byteString) (Right ([UnknownSignature.MkUnknownSignature byteString], Signatures.MkSignatures []))

  Test.describe test "render" $ do
    Test.it test "renders no signatures" $ do
      let signatures = Signatures.MkSignatures []
      Test.assertEq test (Signatures.render signatures) ByteString.empty

    Test.it test "renders one signature" $ do
      let signatures = Signatures.MkSignatures . pure . Signature.Symmetric . SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      Test.assertEq test (Signatures.render signatures) (Ascii.pack "v1,47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=")

    Test.it test "renders many signatures" $ do
      let symmetricSignature = SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      asymmetricSignature <- Either.throw . fmap AsymmetricSignature.MkAsymmetricSignature . Error.eitherCryptoError . Ed25519.signature $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789-abcdefghijklmnopqrstuvqxyz"
      let signatures =
            Signatures.MkSignatures
              [ Signature.Symmetric symmetricSignature,
                Signature.Asymmetric asymmetricSignature
              ]
      Test.assertEq test (Signatures.render signatures) (Ascii.pack "v1,47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU= v1a,QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVotMDEyMzQ1Njc4OS1hYmNkZWZnaGlqa2xtbm9wcXJzdHV2cXh5eg==")
