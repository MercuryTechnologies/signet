module Signet.Unstable.Extra.EitherTest where

import qualified Control.Monad.Catch as Exception
import qualified Data.Void as Void
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Extra.Tasty as Tasty
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Extra.Either" $ do
  Tasty.describe "hush" $ do
    Tasty.it "works with Left" $ do
      Either.hush (Left () :: Either () Void.Void) @?= Nothing

    Tasty.it "works with Right" $ do
      Either.hush (Right () :: Either Void.Void ()) @?= Just ()

  Tasty.describe "throw" $ do
    Tasty.it "throws an exception for Left" $ do
      result <- Exception.try . Either.throw $ Left MkTestException
      result @?= (Left MkTestException :: Either TestException Void.Void)

    Tasty.it "returns the value for Right" $ do
      value <- Either.throw (Right () :: Either Void.Void ())
      value @?= ()

data TestException
  = MkTestException
  deriving (Eq, Show)

instance Exception.Exception TestException
