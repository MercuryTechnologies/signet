module Signet.Unstable.Exception.SignetException where

import qualified Control.Monad.Catch as Exception
import qualified Signet.Unstable.Exception.ToleranceException as ToleranceException
import qualified Signet.Unstable.Exception.VerificationException as VerificationException

data SignetException
  = ToleranceException ToleranceException.ToleranceException
  | VerificationException VerificationException.VerificationException
  deriving (Eq, Show)

instance Exception.Exception SignetException where
  displayException signetException =
    "failed to verify webhook: " <> case signetException of
      ToleranceException toleranceException ->
        Exception.displayException toleranceException
      VerificationException verificationException ->
        Exception.displayException verificationException
