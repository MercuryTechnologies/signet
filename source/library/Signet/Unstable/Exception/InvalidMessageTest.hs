module Signet.Unstable.Exception.InvalidMessageTest where

import qualified Heck as Test

spec :: (Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Exception.InvalidMessage" $ do
  pure ()
