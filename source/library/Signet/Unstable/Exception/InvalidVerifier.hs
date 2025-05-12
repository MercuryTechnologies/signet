module Signet.Unstable.Exception.InvalidVerifier where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString

newtype InvalidVerifier
  = MkInvalidVerifier ByteString.ByteString
  deriving (Eq, Show)

instance Exception.Exception InvalidVerifier where
  displayException = mappend "invalid verifier: " . show . unwrap

unwrap :: InvalidVerifier -> ByteString.ByteString
unwrap (MkInvalidVerifier byteString) = byteString
