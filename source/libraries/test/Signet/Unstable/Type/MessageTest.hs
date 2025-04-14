module Signet.Unstable.Type.MessageTest where

import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidMessage as InvalidMessage
import qualified Signet.Unstable.Exception.InvalidTimestamp as InvalidTimestamp
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Extra.Tasty as Tasty
import qualified Signet.Unstable.Type.Id as Id
import qualified Signet.Unstable.Type.Message as Message
import qualified Signet.Unstable.Type.Payload as Payload
import qualified Signet.Unstable.Type.Timestamp as Timestamp
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Type.Message" $ do
  Tasty.describe "parse" $ do
    Tasty.it "fails with invalid timestamp" $ do
      let timestamp = Ascii.pack "invalid"
      let byteString = Ascii.pack "i." <> timestamp <> Ascii.pack ".p"
      let result = Message.parse byteString
      result @?= Left (InvalidMessage.InvalidTimestamp $ InvalidTimestamp.MkInvalidTimestamp timestamp)

    Tasty.it "succeeds with valid input" $ do
      let result = Message.parse $ Ascii.pack "i.0.p"
      id_ <- Either.throw . Id.parse $ Ascii.pack "i"
      timestamp <- Either.throw . Timestamp.parse $ Ascii.pack "0"
      let payload = Payload.MkPayload $ Ascii.pack "p"
      result
        @?= Right
          Message.MkMessage
            { Message.id_ = id_,
              Message.timestamp = timestamp,
              Message.payload = payload
            }

  Tasty.describe "render" $ do
    Tasty.it "returns the correct ByteString representation" $ do
      id_ <- Either.throw . Id.parse $ Ascii.pack "i"
      timestamp <- Either.throw . Timestamp.parse $ Ascii.pack "0"
      let payload = Payload.MkPayload $ Ascii.pack "p"
      let message =
            Message.MkMessage
              { Message.id_ = id_,
                Message.timestamp = timestamp,
                Message.payload = payload
              }
      Message.render message @?= Ascii.pack "i.0.p"
