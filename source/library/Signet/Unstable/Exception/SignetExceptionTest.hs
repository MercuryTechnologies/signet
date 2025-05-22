module Signet.Unstable.Exception.SignetExceptionTest where

import qualified Heck as Test

spec :: (Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Exception.SignetException" $ do
  pure ()
