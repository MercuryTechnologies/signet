module Signet.Unstable.Type.Id where

import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.Word as Word
import qualified Signet.Unstable.Exception.InvalidId as InvalidId

newtype Id
  = MkId ByteString.ByteString
  deriving (Eq, Show)

unwrap :: Id -> ByteString.ByteString
unwrap (MkId byteString) = byteString

separator :: Word.Word8
separator = 0x2e

parse :: ByteString.ByteString -> Either InvalidId.InvalidId Id
parse byteString = do
  Monad.when (ByteString.any (== separator) byteString)
    . Left
    $ InvalidId.MkInvalidId byteString
  Right $ MkId byteString

render :: Id -> ByteString.ByteString
render = unwrap
