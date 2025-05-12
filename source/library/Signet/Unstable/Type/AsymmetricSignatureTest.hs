module Signet.Unstable.Type.AsymmetricSignatureTest where

import qualified Crypto.Error as Error
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidAsymmetricSignature as InvalidAsymmetricSignature
import qualified Signet.Unstable.Extra.Tasty as Tasty
import qualified Signet.Unstable.Type.AsymmetricSignature as AsymmetricSignature
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Type.AsymmetricSignature" $ do
  Tasty.describe "parse" $ do
    Tasty.it "fails with invalid input" $ do
      let byteString = Ascii.pack "invalid"
      let result = AsymmetricSignature.parse byteString
      result @?= Left (InvalidAsymmetricSignature.MkInvalidAsymmetricSignature byteString)

    Tasty.it "succeeds with valid input" $ do
      let result = AsymmetricSignature.parse $ Ascii.pack "QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVotMDEyMzQ1Njc4OS1hYmNkZWZnaGlqa2xtbm9wcXJzdHV2cXh5eg=="
      signature <- Error.throwCryptoErrorIO . Ed25519.signature $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789-abcdefghijklmnopqrstuvqxyz"
      result @?= Right (AsymmetricSignature.MkAsymmetricSignature signature)

  Tasty.describe "render" $ do
    Tasty.it "works" $ do
      asymmetricSignature <- fmap AsymmetricSignature.MkAsymmetricSignature . Error.throwCryptoErrorIO . Ed25519.signature $ Ascii.pack "ABCDEFGHIJKLMNOPQRSTUVWXYZ-0123456789-abcdefghijklmnopqrstuvqxyz"
      AsymmetricSignature.render asymmetricSignature @?= Ascii.pack "QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVotMDEyMzQ1Njc4OS1hYmNkZWZnaGlqa2xtbm9wcXJzdHV2cXh5eg=="
