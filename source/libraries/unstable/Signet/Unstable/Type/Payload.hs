module Signet.Unstable.Type.Payload where

import qualified Data.ByteString as ByteString

newtype Payload
  = MkPayload ByteString.ByteString
  deriving (Eq, Show)

unwrap :: Payload -> ByteString.ByteString
unwrap (MkPayload byteString) = byteString
