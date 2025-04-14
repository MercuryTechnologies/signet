module Signet.Unstable.Type.UnknownSignature where

import qualified Data.ByteString as ByteString

newtype UnknownSignature
  = MkUnknownSignature ByteString.ByteString
  deriving (Eq, Show)

unwrap :: UnknownSignature -> ByteString.ByteString
unwrap (MkUnknownSignature byteString) = byteString
