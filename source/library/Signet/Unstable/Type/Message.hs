module Signet.Unstable.Type.Message where

import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Signet.Unstable.Exception.InvalidMessage as InvalidMessage
import qualified Signet.Unstable.Type.Id as Id
import qualified Signet.Unstable.Type.Payload as Payload
import qualified Signet.Unstable.Type.Timestamp as Timestamp

data Message = MkMessage
  { id_ :: Id.Id,
    timestamp :: Timestamp.Timestamp,
    payload :: Payload.Payload
  }
  deriving (Eq, Show)

parse :: ByteString.ByteString -> Either InvalidMessage.InvalidMessage Message
parse x = do
  let (rawId, y) = ByteString.break (== Id.separator) x
  theId <- Bifunctor.first InvalidMessage.InvalidId $ Id.parse rawId
  let (rawTimestamp, z) =
        ByteString.break (== Id.separator) $
          ByteString.drop 1 y
  theTimestamp <-
    Bifunctor.first InvalidMessage.InvalidTimestamp $
      Timestamp.parse rawTimestamp
  let thePayload = Payload.MkPayload $ ByteString.drop 1 z
  pure MkMessage {id_ = theId, timestamp = theTimestamp, payload = thePayload}

render :: Message -> ByteString.ByteString
render message =
  Id.render (id_ message)
    <> ByteString.singleton Id.separator
    <> Timestamp.render (timestamp message)
    <> ByteString.singleton Id.separator
    <> Payload.unwrap (payload message)
