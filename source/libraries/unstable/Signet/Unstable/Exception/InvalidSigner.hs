module Signet.Unstable.Exception.InvalidSigner where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString

newtype InvalidSigner
  = MkInvalidSigner ByteString.ByteString
  deriving (Eq, Show)

instance Exception.Exception InvalidSigner where
  displayException = mappend "invalid signer: " . show . unwrap

unwrap :: InvalidSigner -> ByteString.ByteString
unwrap (MkInvalidSigner byteString) = byteString
