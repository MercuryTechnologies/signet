module Signet.Unstable.Extra.HttpTest where

import qualified Data.ByteString.Char8 as Ascii
import qualified Data.CaseInsensitive as CI
import qualified Signet.Unstable.Extra.Http as Http
import qualified Signet.Unstable.Extra.Tasty as Tasty
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Extra.Http" $ do
  Tasty.describe "hWebhookId" $ do
    Tasty.it "creates the correct header name" $ do
      Http.hWebhookId @?= CI.mk (Ascii.pack "webhook-id")

  Tasty.describe "hWebhookSignature" $ do
    Tasty.it "creates the correct header name" $ do
      Http.hWebhookSignature @?= CI.mk (Ascii.pack "webhook-signature")

  Tasty.describe "hWebhookTimestamp" $ do
    Tasty.it "creates the correct header name" $ do
      Http.hWebhookTimestamp @?= CI.mk (Ascii.pack "webhook-timestamp")
