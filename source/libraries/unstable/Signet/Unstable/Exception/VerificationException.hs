module Signet.Unstable.Exception.VerificationException where

import qualified Control.Monad.Catch as Exception
import qualified Signet.Unstable.Type.Id as Id

newtype VerificationException
  = MkVerificationException Id.Id
  deriving (Eq, Show)

instance Exception.Exception VerificationException where
  displayException = mappend "verification failed: " . show . unwrap

unwrap :: VerificationException -> Id.Id
unwrap (MkVerificationException id_) = id_
