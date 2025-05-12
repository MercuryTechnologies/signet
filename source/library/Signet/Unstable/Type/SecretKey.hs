module Signet.Unstable.Type.SecretKey where

import qualified Crypto.Error as Error
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray.Encoding as Encoding
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidSecretKey as InvalidSecretKey
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Extra.Maybe as Maybe

newtype SecretKey
  = MkSecretKey Ed25519.SecretKey
  deriving (Eq, Show)

unwrap :: SecretKey -> Ed25519.SecretKey
unwrap (MkSecretKey secretKey) = secretKey

prefix :: ByteString.ByteString
prefix = Ascii.pack "whsk_"

parse :: ByteString.ByteString -> Either InvalidSecretKey.InvalidSecretKey SecretKey
parse prefixed = Maybe.note (InvalidSecretKey.MkInvalidSecretKey prefixed) $ do
  encoded <- ByteString.stripPrefix prefix prefixed
  byteString <- Either.hush $ Encoding.convertFromBase Encoding.Base64 encoded
  fmap MkSecretKey
    . Error.maybeCryptoError
    $ Ed25519.secretKey (byteString :: ByteString.ByteString)

render :: SecretKey -> ByteString.ByteString
render = mappend prefix . Encoding.convertToBase Encoding.Base64 . unwrap
