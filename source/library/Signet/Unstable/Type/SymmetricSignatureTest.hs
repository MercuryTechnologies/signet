module Signet.Unstable.Type.SymmetricSignatureTest where

import qualified Crypto.Hash as Hash
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Ascii
import qualified Heck as Test
import qualified Signet.Unstable.Exception.InvalidSymmetricSignature as InvalidSymmetricSignature
import qualified Signet.Unstable.Type.SymmetricSignature as SymmetricSignature

spec :: (Applicative io, Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Type.SymmetricSignature" $ do
  Test.describe test "parse" $ do
    Test.it test "fails with invalid symmetric signature" $ do
      let byteString = Ascii.pack "invalid"
      let result = SymmetricSignature.parse byteString
      Test.assertEq test result (Left (InvalidSymmetricSignature.MkInvalidSymmetricSignature byteString))

    Test.it test "succeeds with valid symmetric signature" $ do
      let byteString = Ascii.pack "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU="
      let signature = SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      Test.assertEq test (SymmetricSignature.parse byteString) (Right signature)

  Test.describe test "render" $ do
    Test.it test "renders symmetric signature correctly" $ do
      let signature = SymmetricSignature.MkSymmetricSignature $ Hash.hash ByteString.empty
      Test.assertEq test (SymmetricSignature.render signature) (Ascii.pack "47DEQpj8HBSa+/TImW+5JCeuQeRkm5NMpJWZG3hSuFU=")
