module Signet.Unstable.Type.SymmetricSignature where

import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteArray.Encoding as Encoding
import qualified Data.ByteString as ByteString
import qualified Data.Function as Function
import qualified Signet.Unstable.Exception.InvalidSymmetricSignature as InvalidSymmetricSignature
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Extra.Maybe as Maybe

newtype SymmetricSignature
  = MkSymmetricSignature (Hash.Digest Hash.SHA256)
  deriving (Show)

instance Eq SymmetricSignature where
  (==) = Function.on ByteArray.constEq unwrap

unwrap :: SymmetricSignature -> Hash.Digest Hash.SHA256
unwrap (MkSymmetricSignature digest) = digest

parse :: ByteString.ByteString -> Either InvalidSymmetricSignature.InvalidSymmetricSignature SymmetricSignature
parse encoded = Maybe.note (InvalidSymmetricSignature.MkInvalidSymmetricSignature encoded) $ do
  byteString <- Either.hush $ Encoding.convertFromBase Encoding.Base64 encoded
  MkSymmetricSignature <$> Hash.digestFromByteString (byteString :: ByteString.ByteString)

render :: SymmetricSignature -> ByteString.ByteString
render = Encoding.convertToBase Encoding.Base64 . unwrap
