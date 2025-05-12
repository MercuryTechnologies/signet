module Signet.Unstable.Type.SignatureTest where

import qualified Crypto.Error as Error
import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidAsymmetricSignature as InvalidAsymmetricSignature
import qualified Signet.Unstable.Exception.InvalidSignature as InvalidSignature
import qualified Signet.Unstable.Exception.InvalidSymmetricSignature as InvalidSymmetricSignature
import qualified Signet.Unstable.Extra.Tasty as Tasty
import qualified Signet.Unstable.Type.AsymmetricSignature as AsymmetricSignature
import qualified Signet.Unstable.Type.Signature as Signature
import qualified Signet.Unstable.Type.SymmetricSignature as SymmetricSignature
import qualified Signet.Unstable.Type.UnknownSignature as UnknownSignature
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Type.Signature" $ do
  Tasty.describe "parse" $ do
    Tasty.it "fails with invalid asymmetric signature" $ do
      let byteString = Ascii.pack "invalid"
      let result = Signature.parse $ Ascii.pack "v1a," <> byteString
      result @?= Left (InvalidSignature.InvalidAsymmetricSignature $ InvalidAsymmetricSignature.MkInvalidAsymmetricSignature byteString)

    Tasty.it "fails with invalid symmetric signature" $ do
      let byteString = Ascii.pack "invalid"
      let result = Signature.parse $ Ascii.pack "v1," <> byteString
      result @?= Left (InvalidSignature.InvalidSymmetricSignature $ InvalidSymmetricSignature.MkInvalidSymmetricSignature byteString)

    Tasty.it "returns unknown signature with unrecognized prefix" $ do
      let byteString = Ascii.pack "unknown"
      let result = Signature.parse byteString
      result @?= Right (Left $ UnknownSignature.MkUnknownSignature byteString)

    Tasty.it "succeeds with valid asymmetric signature" $ do
      let byteString = Ascii.pack "v1a,QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVotMDEyMzQ1Njc4OS1hYmNkZWZnaGlqa2xtbm9wcXJzdHV2cXh5eg=="
      signature <- fmap (Signature.Asymmetric . AsymmetricSignature.MkAsymmetricSignature) . Error.throwCryptoErrorIO . Ed25519.signature $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789-abcdefghijklmnopqrstuvqxyz"
      Signature.parse byteString @?= Right (Right signature)

    Tasty.it "succeeds with valid symmetric signature" $ do
      let byteString = Ascii.pack "v1,47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
      let signature = Signature.Symmetric . SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      Signature.parse byteString @?= Right (Right signature)

  Tasty.describe "render" $ do
    Tasty.it "renders asymmetric signature correctly" $ do
      signature <- fmap (Signature.Asymmetric . AsymmetricSignature.MkAsymmetricSignature) . Error.throwCryptoErrorIO . Ed25519.signature $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789-abcdefghijklmnopqrstuvqxyz"
      Signature.render signature @?= Ascii.pack "v1a,QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVotMDEyMzQ1Njc4OS1hYmNkZWZnaGlqa2xtbm9wcXJzdHV2cXh5eg=="

    Tasty.it "renders symmetric signature correctly" $ do
      let signature = Signature.Symmetric . SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      Signature.render signature @?= Ascii.pack "v1,47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
