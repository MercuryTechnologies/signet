module Signet.Unstable.Exception.InvalidPublicKey where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString

newtype InvalidPublicKey
  = MkInvalidPublicKey ByteString.ByteString
  deriving (Eq, Show)

instance Exception.Exception InvalidPublicKey where
  displayException = mappend "invalid public key: " . show . unwrap

unwrap :: InvalidPublicKey -> ByteString.ByteString
unwrap (MkInvalidPublicKey byteString) = byteString
