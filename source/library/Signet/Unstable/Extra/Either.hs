module Signet.Unstable.Extra.Either where

import qualified Control.Monad.Catch as Exception

hush :: Either x a -> Maybe a
hush = either (const Nothing) Just

throw :: (Exception.Exception e, Exception.MonadThrow m) => Either e a -> m a
throw = either Exception.throwM pure
