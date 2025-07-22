module Signet.Unstable.Extra.HttpTest where

import qualified Data.ByteString.Char8 as Ascii
import qualified Data.CaseInsensitive as CI
import qualified Heck as Test
import qualified Signet.Unstable.Extra.Http as Http

spec :: (Applicative io, Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Extra.Http" $ do
  Test.describe test "hWebhookId" $ do
    Test.it test "creates the correct header name" $ do
      Test.assertEq test Http.hWebhookId (CI.mk (Ascii.pack "webhook-id"))

  Test.describe test "hWebhookSignature" $ do
    Test.it test "creates the correct header name" $ do
      Test.assertEq test Http.hWebhookSignature (CI.mk (Ascii.pack "webhook-signature"))

  Test.describe test "hWebhookTimestamp" $ do
    Test.it test "creates the correct header name" $ do
      Test.assertEq test Http.hWebhookTimestamp (CI.mk (Ascii.pack "webhook-timestamp"))
