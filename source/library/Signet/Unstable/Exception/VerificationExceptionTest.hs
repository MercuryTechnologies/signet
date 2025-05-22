module Signet.Unstable.Exception.VerificationExceptionTest where

import qualified Heck as Test

spec :: (Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Exception.VerificationException" $ do
  pure ()
