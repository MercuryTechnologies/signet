module Signet.Unstable.Exception.InvalidMessage where

import qualified Control.Monad.Catch as Exception
import qualified Signet.Unstable.Exception.InvalidId as InvalidId
import qualified Signet.Unstable.Exception.InvalidTimestamp as InvalidTimestamp

data InvalidMessage
  = InvalidId InvalidId.InvalidId
  | InvalidTimestamp InvalidTimestamp.InvalidTimestamp
  deriving (Eq, Show)

instance Exception.Exception InvalidMessage where
  displayException invalidMessage =
    "invalid message: " <> case invalidMessage of
      InvalidId invalidId ->
        Exception.displayException invalidId
      InvalidTimestamp invalidTimestamp ->
        Exception.displayException invalidTimestamp
