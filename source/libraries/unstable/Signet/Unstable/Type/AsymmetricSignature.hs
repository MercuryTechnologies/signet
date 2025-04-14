module Signet.Unstable.Type.AsymmetricSignature where

import qualified Crypto.Error as Error
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray.Encoding as Encoding
import qualified Data.ByteString as ByteString
import qualified Signet.Unstable.Exception.InvalidAsymmetricSignature as InvalidAsymmetricSignature
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Extra.Maybe as Maybe

newtype AsymmetricSignature
  = MkAsymmetricSignature Ed25519.Signature
  deriving (Eq, Show)

unwrap :: AsymmetricSignature -> Ed25519.Signature
unwrap (MkAsymmetricSignature signature) = signature

parse ::
  ByteString.ByteString ->
  Either InvalidAsymmetricSignature.InvalidAsymmetricSignature AsymmetricSignature
parse encoded = Maybe.note (InvalidAsymmetricSignature.MkInvalidAsymmetricSignature encoded) $ do
  byteString <- Either.hush $ Encoding.convertFromBase Encoding.Base64 encoded
  fmap MkAsymmetricSignature
    . Error.maybeCryptoError
    $ Ed25519.signature (byteString :: ByteString.ByteString)

render :: AsymmetricSignature -> ByteString.ByteString
render = Encoding.convertToBase Encoding.Base64 . unwrap
