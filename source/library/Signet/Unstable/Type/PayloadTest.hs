module Signet.Unstable.Type.PayloadTest where

import qualified Heck as Test

spec :: (Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Type.Payload" $ do
  pure ()
