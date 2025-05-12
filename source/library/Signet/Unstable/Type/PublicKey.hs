module Signet.Unstable.Type.PublicKey where

import qualified Crypto.Error as Error
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray.Encoding as Encoding
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidPublicKey as InvalidPublicKey
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Extra.Maybe as Maybe

newtype PublicKey
  = MkPublicKey Ed25519.PublicKey
  deriving (Eq, Show)

unwrap :: PublicKey -> Ed25519.PublicKey
unwrap (MkPublicKey publicKey) = publicKey

prefix :: ByteString.ByteString
prefix = Ascii.pack "whpk_"

parse :: ByteString.ByteString -> Either InvalidPublicKey.InvalidPublicKey PublicKey
parse prefixed = Maybe.note (InvalidPublicKey.MkInvalidPublicKey prefixed) $ do
  encoded <- ByteString.stripPrefix prefix prefixed
  byteString <- Either.hush $ Encoding.convertFromBase Encoding.Base64 encoded
  fmap MkPublicKey
    . Error.maybeCryptoError
    $ Ed25519.publicKey (byteString :: ByteString.ByteString)

render :: PublicKey -> ByteString.ByteString
render = mappend prefix . Encoding.convertToBase Encoding.Base64 . unwrap
