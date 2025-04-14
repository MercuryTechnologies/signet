module Signet.Unstable.Extra.MaybeTest where

import qualified Data.Void as Void
import qualified Signet.Unstable.Extra.Maybe as Maybe
import qualified Signet.Unstable.Extra.Tasty as Tasty
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Extra.Maybe" $ do
  Tasty.describe "note" $ do
    Tasty.it "works with nothing" $ do
      Maybe.note () (Nothing :: Maybe Void.Void) @?= Left ()

    Tasty.it "works with just" $ do
      Maybe.note () (Just True) @?= Right True
