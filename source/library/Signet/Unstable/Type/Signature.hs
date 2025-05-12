module Signet.Unstable.Type.Signature where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Ascii
import qualified Data.Word as Word
import qualified Signet.Unstable.Exception.InvalidSignature as InvalidSignature
import qualified Signet.Unstable.Type.AsymmetricSignature as AsymmetricSignature
import qualified Signet.Unstable.Type.SymmetricSignature as SymmetricSignature
import qualified Signet.Unstable.Type.UnknownSignature as UnknownSignature

data Signature
  = Asymmetric AsymmetricSignature.AsymmetricSignature
  | Symmetric SymmetricSignature.SymmetricSignature
  deriving (Eq, Show)

separator :: Word.Word8
separator = 0x2c

asymmetricPrefix :: ByteString.ByteString
asymmetricPrefix = Ascii.pack "v1a"

symmetricPrefix :: ByteString.ByteString
symmetricPrefix = Ascii.pack "v1"

parse :: ByteString.ByteString -> Either InvalidSignature.InvalidSignature (Either UnknownSignature.UnknownSignature Signature)
parse prefixed = do
  let (prefix, rest) = ByteString.break (== separator) prefixed
  case ByteString.drop 1 rest of
    byteString
      | prefix == asymmetricPrefix ->
          Bifunctor.bimap InvalidSignature.InvalidAsymmetricSignature (Right . Asymmetric) $
            AsymmetricSignature.parse byteString
      | prefix == symmetricPrefix ->
          Bifunctor.bimap InvalidSignature.InvalidSymmetricSignature (Right . Symmetric) $
            SymmetricSignature.parse byteString
    _ -> Right . Left $ UnknownSignature.MkUnknownSignature prefixed

render :: Signature -> ByteString.ByteString
render signature =
  case signature of
    Asymmetric asymmetricSignature ->
      asymmetricPrefix
        <> ByteString.singleton separator
        <> AsymmetricSignature.render asymmetricSignature
    Symmetric symmetricSignature ->
      symmetricPrefix
        <> ByteString.singleton separator
        <> SymmetricSignature.render symmetricSignature
