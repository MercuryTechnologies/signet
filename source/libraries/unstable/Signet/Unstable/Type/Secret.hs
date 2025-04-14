module Signet.Unstable.Type.Secret where

import qualified Data.ByteArray as ByteArray
import qualified Data.ByteArray.Encoding as Encoding
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidSecret as InvalidSecret
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Extra.Maybe as Maybe

newtype Secret
  = MkSecret ByteArray.ScrubbedBytes
  deriving (Eq, Show)

unwrap :: Secret -> ByteArray.ScrubbedBytes
unwrap (MkSecret scrubbedBytes) = scrubbedBytes

prefix :: ByteString.ByteString
prefix = Ascii.pack "whsec_"

parse :: ByteString.ByteString -> Either InvalidSecret.InvalidSecret Secret
parse prefixed = Maybe.note (InvalidSecret.MkInvalidSecret prefixed) $ do
  encoded <- ByteString.stripPrefix prefix prefixed
  fmap MkSecret . Either.hush $ Encoding.convertFromBase Encoding.Base64 encoded

render :: Secret -> ByteString.ByteString
render = mappend prefix . Encoding.convertToBase Encoding.Base64 . unwrap
