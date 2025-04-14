module Signet.Unstable.Type.SymmetricSignatureTest where

import qualified Crypto.Hash as Hash
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidSymmetricSignature as InvalidSymmetricSignature
import qualified Signet.Unstable.Extra.Tasty as Tasty
import qualified Signet.Unstable.Type.SymmetricSignature as SymmetricSignature
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Type.SymmetricSignature" $ do
  Tasty.describe "parse" $ do
    Tasty.it "fails with invalid symmetric signature" $ do
      let byteString = Ascii.pack "invalid"
      let result = SymmetricSignature.parse byteString
      result @?= Left (InvalidSymmetricSignature.MkInvalidSymmetricSignature byteString)

    Tasty.it "succeeds with valid symmetric signature" $ do
      let byteString = Ascii.pack "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
      let signature = SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      SymmetricSignature.parse byteString @?= Right signature

  Tasty.describe "render" $ do
    Tasty.it "renders symmetric signature correctly" $ do
      let signature = SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      SymmetricSignature.render signature @?= Ascii.pack "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
