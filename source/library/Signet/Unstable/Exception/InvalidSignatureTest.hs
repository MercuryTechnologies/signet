module Signet.Unstable.Exception.InvalidSignatureTest where

import qualified Heck as Test

spec :: (Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Exception.InvalidSignature" $ do
  pure ()
