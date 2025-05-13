{-# LANGUAGE RankNTypes #-}

module Signet.Unstable.Type.Test where

import qualified Control.Monad as Monad
import qualified GHC.Stack as Stack

data Test io tree = MkTest
  { assertFailure :: forall void. (Stack.HasCallStack) => String -> io void,
    describe :: String -> tree () -> tree (),
    it :: String -> io () -> tree ()
  }

assertEq ::
  (Stack.HasCallStack, Applicative io, Eq a, Show a) =>
  Test io tree ->
  a ->
  a ->
  io ()
assertEq test expected actual =
  Monad.when (expected /= actual)
    . assertFailure test
    $ "expected " <> show expected <> " but got " <> show actual
