module Signet.Unstable.Extra.Http where

import qualified Data.ByteString.Char8 as Ascii
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types as Http

hWebhookId :: Http.HeaderName
hWebhookId = CI.mk $ Ascii.pack "webhook-id"

hWebhookSignature :: Http.HeaderName
hWebhookSignature = CI.mk $ Ascii.pack "webhook-signature"

hWebhookTimestamp :: Http.HeaderName
hWebhookTimestamp = CI.mk $ Ascii.pack "webhook-timestamp"
