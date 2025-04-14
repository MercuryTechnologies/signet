module Signet.Unstable.Exception.InvalidSecret where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString as ByteString

newtype InvalidSecret
  = MkInvalidSecret ByteString.ByteString
  deriving (Eq, Show)

instance Exception.Exception InvalidSecret where
  displayException = mappend "invalid secret: " . show . unwrap

unwrap :: InvalidSecret -> ByteString.ByteString
unwrap (MkInvalidSecret byteString) = byteString
