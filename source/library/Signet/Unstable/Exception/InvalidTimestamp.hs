module Signet.Unstable.Exception.InvalidTimestamp where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString

newtype InvalidTimestamp
  = MkInvalidTimestamp ByteString.ByteString
  deriving (Eq, Show)

instance Exception.Exception InvalidTimestamp where
  displayException = mappend "invalid timestamp: " . show . unwrap

unwrap :: InvalidTimestamp -> ByteString.ByteString
unwrap (MkInvalidTimestamp byteString) = byteString
