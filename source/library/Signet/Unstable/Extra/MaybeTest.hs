module Signet.Unstable.Extra.MaybeTest where

import qualified Data.Void as Void
import qualified Signet.Unstable.Extra.Maybe as Maybe
import qualified Signet.Unstable.Type.Test as Test

spec :: (Applicative io, Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Extra.Maybe" $ do
  Test.describe test "note" $ do
    Test.it test "works with nothing" $ do
      Test.assertEq test (Maybe.note () (Nothing :: Maybe Void.Void)) (Left ())

    Test.it test "works with just" $ do
      Test.assertEq test (Maybe.note () (Just True)) (Right True)
