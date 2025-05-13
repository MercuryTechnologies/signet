module Signet.Unstable.Exception.UnknownSignature where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString

newtype UnknownSignature
  = MkUnknownSignature ByteString.ByteString
  deriving (Eq, Show)

instance Exception.Exception UnknownSignature where
  displayException = mappend "unknown signature: " . show . unwrap

unwrap :: UnknownSignature -> ByteString.ByteString
unwrap (MkUnknownSignature byteString) = byteString
