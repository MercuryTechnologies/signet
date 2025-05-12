module Signet.Unstable.Type.UnknownSignatureTest where

import qualified Signet.Unstable.Type.Test as Test

spec :: (Monad tree) => Test.Test tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Type.UnknownSignature" $ do
  pure ()
