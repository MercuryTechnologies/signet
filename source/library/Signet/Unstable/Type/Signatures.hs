module Signet.Unstable.Type.Signatures where

import qualified Data.ByteString as ByteString
import qualified Data.Either as Either
import qualified Data.Word as Word
import qualified Signet.Unstable.Exception.InvalidSignature as InvalidSignature
import qualified Signet.Unstable.Type.Signature as Signature
import qualified Signet.Unstable.Type.UnknownSignature as UnknownSignature

newtype Signatures
  = MkSignatures [Signature.Signature]
  deriving (Eq, Show)

unwrap :: Signatures -> [Signature.Signature]
unwrap (MkSignatures signatures) = signatures

separator :: Word.Word8
separator = 0x20

parse :: ByteString.ByteString -> Either InvalidSignature.InvalidSignature ([UnknownSignature.UnknownSignature], Signatures)
parse =
  fmap (fmap MkSignatures . Either.partitionEithers)
    . traverse Signature.parse
    . ByteString.split separator

render :: Signatures -> ByteString.ByteString
render =
  ByteString.intercalate (ByteString.singleton separator)
    . fmap Signature.render
    . unwrap
