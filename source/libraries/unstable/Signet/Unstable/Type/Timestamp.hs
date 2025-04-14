module Signet.Unstable.Type.Timestamp where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import qualified Signet.Unstable.Exception.InvalidTimestamp as InvalidTimestamp
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Extra.Maybe as Maybe

newtype Timestamp
  = MkTimestamp Time.UTCTime
  deriving (Eq, Show)

unwrap :: Timestamp -> Time.UTCTime
unwrap (MkTimestamp utcTime) = utcTime

format :: String
format = "%s"

parse :: ByteString.ByteString -> Either InvalidTimestamp.InvalidTimestamp Timestamp
parse byteString = Maybe.note (InvalidTimestamp.MkInvalidTimestamp byteString) $ do
  text <- Either.hush $ Text.decodeUtf8' byteString
  fmap MkTimestamp
    . Time.parseTimeM False Time.defaultTimeLocale format
    $ Text.unpack text

render :: Timestamp -> ByteString.ByteString
render =
  Text.encodeUtf8
    . Text.pack
    . Time.formatTime Time.defaultTimeLocale format
    . unwrap
