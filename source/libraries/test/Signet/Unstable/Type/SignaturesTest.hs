module Signet.Unstable.Type.SignaturesTest where

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
import qualified Signet.Unstable.Type.Signatures as Signatures
import qualified Signet.Unstable.Type.SymmetricSignature as SymmetricSignature
import qualified Signet.Unstable.Type.UnknownSignature as UnknownSignature
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Type.Signatures" $ do
  Tasty.describe "parse" $ do
    Tasty.it "succeeds with no signatures" $ do
      Signatures.parse ByteString.empty @?= Right ([], Signatures.MkSignatures [])

    Tasty.it "succeeds with one signature" $ do
      let signature = Signature.Symmetric . SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      let byteString = Ascii.pack "v1,47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
      Signatures.parse byteString @?= Right ([], Signatures.MkSignatures [signature])

    Tasty.it "succeeds with many signatures" $ do
      let symmetricSignature = SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      asymmetricSignature <- fmap AsymmetricSignature.MkAsymmetricSignature . Error.throwCryptoErrorIO . Ed25519.signature $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789-abcdefghijklmnopqrstuvqxyz"
      let byteString = Ascii.pack "v1,47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU= v1a,QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVotMDEyMzQ1Njc4OS1hYmNkZWZnaGlqa2xtbm9wcXJzdHV2cXh5eg=="
      let signatures =
            Signatures.MkSignatures
              [ Signature.Symmetric symmetricSignature,
                Signature.Asymmetric asymmetricSignature
              ]
      Signatures.parse byteString @?= Right ([], signatures)

    Tasty.it "fails with an invalid symmetric signature" $ do
      let x = Ascii.pack "invalid"
      let byteString = Ascii.pack "v1," <> x
      Signatures.parse byteString @?= Left (InvalidSignature.InvalidSymmetricSignature $ InvalidSymmetricSignature.MkInvalidSymmetricSignature x)

    Tasty.it "fails with an invalid asymmetric signature" $ do
      let x = Ascii.pack "invalid"
      let byteString = Ascii.pack "v1a," <> x
      Signatures.parse byteString @?= Left (InvalidSignature.InvalidAsymmetricSignature $ InvalidAsymmetricSignature.MkInvalidAsymmetricSignature x)

    Tasty.it "succeeds with an unknown signature" $ do
      let byteString = Ascii.pack "unknown"
      Signatures.parse byteString @?= Right ([UnknownSignature.MkUnknownSignature byteString], Signatures.MkSignatures [])

  Tasty.describe "render" $ do
    Tasty.it "renders no signatures" $ do
      let signatures = Signatures.MkSignatures []
      Signatures.render signatures @?= ByteString.empty

    Tasty.it "renders one signature" $ do
      let signatures = Signatures.MkSignatures . pure . Signature.Symmetric . SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      Signatures.render signatures @?= Ascii.pack "v1,47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="

    Tasty.it "renders many signatures" $ do
      let symmetricSignature = SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      asymmetricSignature <- fmap AsymmetricSignature.MkAsymmetricSignature . Error.throwCryptoErrorIO . Ed25519.signature $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789-abcdefghijklmnopqrstuvqxyz"
      let signatures =
            Signatures.MkSignatures
              [ Signature.Symmetric symmetricSignature,
                Signature.Asymmetric asymmetricSignature
              ]
      Signatures.render signatures @?= Ascii.pack "v1,47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU= v1a,QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVotMDEyMzQ1Njc4OS1hYmNkZWZnaGlqa2xtbm9wcXJzdHV2cXh5eg=="
