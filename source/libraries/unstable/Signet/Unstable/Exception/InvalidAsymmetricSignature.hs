module Signet.Unstable.Exception.InvalidAsymmetricSignature where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString

newtype InvalidAsymmetricSignature
  = MkInvalidAsymmetricSignature ByteString.ByteString
  deriving (Eq, Show)

instance Exception.Exception InvalidAsymmetricSignature where
  displayException = mappend "invalid asymmetric signature: " . show . unwrap

unwrap :: InvalidAsymmetricSignature -> ByteString.ByteString
unwrap (MkInvalidAsymmetricSignature byteString) = byteString
