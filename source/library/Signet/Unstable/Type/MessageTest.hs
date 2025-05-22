module Signet.Unstable.Type.MessageTest where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString.Char8 as Ascii
import qualified Heck as Test
import qualified Signet.Unstable.Exception.InvalidMessage as InvalidMessage
import qualified Signet.Unstable.Exception.InvalidTimestamp as InvalidTimestamp
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Type.Id as Id
import qualified Signet.Unstable.Type.Message as Message
import qualified Signet.Unstable.Type.Payload as Payload
import qualified Signet.Unstable.Type.Timestamp as Timestamp

spec :: (Exception.MonadThrow io, Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Type.Message" $ do
  Test.describe test "parse" $ do
    Test.it test "fails with invalid timestamp" $ do
      let timestamp = Ascii.pack "invalid"
      let byteString = Ascii.pack "i." <> timestamp <> Ascii.pack ".p"
      let result = Message.parse byteString
      Test.assertEq test result (Left (InvalidMessage.InvalidTimestamp $ InvalidTimestamp.MkInvalidTimestamp timestamp))

    Test.it test "succeeds with valid input" $ do
      let result = Message.parse $ Ascii.pack "i.0.p"
      id_ <- Either.throw . Id.parse $ Ascii.pack "i"
      timestamp <- Either.throw . Timestamp.parse $ Ascii.pack "0"
      let payload = Payload.MkPayload $ Ascii.pack "p"
      Test.assertEq
        test
        result
        ( Right
            Message.MkMessage
              { Message.id_ = id_,
                Message.timestamp = timestamp,
                Message.payload = payload
              }
        )

  Test.describe test "render" $ do
    Test.it test "returns the correct ByteString representation" $ do
      id_ <- Either.throw . Id.parse $ Ascii.pack "i"
      timestamp <- Either.throw . Timestamp.parse $ Ascii.pack "0"
      let payload = Payload.MkPayload $ Ascii.pack "p"
      let message =
            Message.MkMessage
              { Message.id_ = id_,
                Message.timestamp = timestamp,
                Message.payload = payload
              }
      Test.assertEq test (Message.render message) (Ascii.pack "i.0.p")
