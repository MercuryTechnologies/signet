module Signet.Unstable.Exception.InvalidSecretKey where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString

newtype InvalidSecretKey
  = MkInvalidSecretKey ByteString.ByteString
  deriving (Eq, Show)

instance Exception.Exception InvalidSecretKey where
  displayException = mappend "invalid secret key: " . show . unwrap

unwrap :: InvalidSecretKey -> ByteString.ByteString
unwrap (MkInvalidSecretKey byteString) = byteString
