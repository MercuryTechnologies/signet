module Signet.Unstable.Extra.EitherTest where

import qualified Control.Monad.Catch as Exception
import qualified Data.Void as Void
import qualified Heck as Test
import qualified Signet.Unstable.Extra.Either as Either

spec :: (Exception.MonadCatch io, Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Extra.Either" $ do
  Test.describe test "hush" $ do
    Test.it test "works with Left" $ do
      Test.assertEq test (Either.hush (Left () :: Either () Void.Void)) Nothing

    Test.it test "works with Right" $ do
      Test.assertEq test (Either.hush (Right () :: Either Void.Void ())) (Just ())

  Test.describe test "throw" $ do
    Test.it test "throws an exception for Left" $ do
      result <- Exception.try . Either.throw $ Left MkTestException
      Test.assertEq test result (Left MkTestException :: Either TestException Void.Void)

    Test.it test "returns the value for Right" $ do
      value <- Either.throw (Right () :: Either Void.Void ())
      Test.assertEq test value ()

data TestException
  = MkTestException
  deriving (Eq, Show)

instance Exception.Exception TestException
