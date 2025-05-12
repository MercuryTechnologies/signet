{-# LANGUAGE RankNTypes #-}

module Signet.Unstable.Type.Test where

import qualified Control.Monad as Monad
import qualified Data.Void as Void
import qualified GHC.Stack as Stack

data Test spec = MkTest
  { assertFailure :: (Stack.HasCallStack) => String -> IO Void.Void,
    describe :: String -> spec () -> spec (),
    it :: String -> IO () -> spec ()
  }

assertEq :: (Stack.HasCallStack, Eq a, Show a) => Test tree -> a -> a -> IO ()
assertEq test expected actual =
  Monad.when (expected /= actual)
    . Monad.void
    . assertFailure test
    $ "expected " <> show expected <> " but got " <> show actual
