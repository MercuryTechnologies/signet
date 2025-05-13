module Signet.Unstable.Exception.InvalidTimestampTest where

import qualified Signet.Unstable.Type.Test as Test

spec :: (Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Exception.InvalidTimestamp" $ do
  pure ()
