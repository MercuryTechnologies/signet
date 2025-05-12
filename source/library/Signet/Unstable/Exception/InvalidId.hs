module Signet.Unstable.Exception.InvalidId where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString

newtype InvalidId
  = MkInvalidId ByteString.ByteString
  deriving (Eq, Show)

instance Exception.Exception InvalidId where
  displayException = mappend "invalid ID: " . show . unwrap

unwrap :: InvalidId -> ByteString.ByteString
unwrap (MkInvalidId byteString) = byteString
