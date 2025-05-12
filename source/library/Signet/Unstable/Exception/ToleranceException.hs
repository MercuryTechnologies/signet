module Signet.Unstable.Exception.ToleranceException where

import qualified Control.Monad.Catch as Exception
import qualified Signet.Unstable.Type.Timestamp as Timestamp

newtype ToleranceException
  = MkToleranceException Timestamp.Timestamp
  deriving (Eq, Show)

instance Exception.Exception ToleranceException where
  displayException = mappend "timestamp out of tolerance: " . show . unwrap

unwrap :: ToleranceException -> Timestamp.Timestamp
unwrap (MkToleranceException timestamp) = timestamp
