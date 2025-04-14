module Signet.Unstable.Exception.InvalidSymmetricSignature where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString

newtype InvalidSymmetricSignature
  = MkInvalidSymmetricSignature ByteString.ByteString
  deriving (Eq, Show)

instance Exception.Exception InvalidSymmetricSignature where
  displayException = mappend "invalid symmetric signature: " . show . unwrap

unwrap :: InvalidSymmetricSignature -> ByteString.ByteString
unwrap (MkInvalidSymmetricSignature byteString) = byteString
