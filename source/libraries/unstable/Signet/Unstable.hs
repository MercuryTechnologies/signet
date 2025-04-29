{-# LANGUAGE PatternSynonyms #-}

module Signet.Unstable where

import qualified Control.Monad.Catch as Exception
import qualified Control.Monad.IO.Class as MonadIO
import qualified Data.Bifunctor as Bifunctor
import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Time as Time
import qualified Signet.Unstable.Exception.InvalidId as InvalidId
import qualified Signet.Unstable.Exception.InvalidMessage as InvalidMessage
import qualified Signet.Unstable.Exception.InvalidSignature as InvalidSignature
import qualified Signet.Unstable.Exception.InvalidSigner as InvalidSigner
import qualified Signet.Unstable.Exception.InvalidTimestamp as InvalidTimestamp
import qualified Signet.Unstable.Exception.InvalidVerifier as InvalidVerifier
import qualified Signet.Unstable.Exception.SignetException as SignetException
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Type.Id as Id
import qualified Signet.Unstable.Type.Message as Message
import qualified Signet.Unstable.Type.Payload as Payload
import qualified Signet.Unstable.Type.PublicKey as PublicKey
import qualified Signet.Unstable.Type.Secret as Secret
import qualified Signet.Unstable.Type.SecretKey as SecretKey
import qualified Signet.Unstable.Type.Signature as Signature
import qualified Signet.Unstable.Type.Signatures as Signatures
import qualified Signet.Unstable.Type.Signer as Signer
import qualified Signet.Unstable.Type.Timestamp as Timestamp
import qualified Signet.Unstable.Type.Tolerance as Tolerance
import qualified Signet.Unstable.Type.UnknownSignature as UnknownSignature
import qualified Signet.Unstable.Type.Verifier as Verifier

-- | Verifies a webhook with 'Text.Text' values. This is a wrapper around
-- 'verifyWebhookByteString' that assumes all values are encoded as UTF-8.
verifyWebhookText ::
  (MonadIO.MonadIO m, Exception.MonadThrow m) =>
  -- | A 'Verifier.Verifier' for the webhook. Typically this will be
  -- @"whsec_..."@ or @"whpk_..."@.
  Text.Text ->
  -- | The webhook's unique 'Id.Id'. Typically this will come from the
  -- 'Signet.Unstable.Extra.Http.hWebhookId' header and look like @"msg_..."@.
  Text.Text ->
  -- | The webhook's 'Timestamp.Timestamp'. This is an integer number of
  -- seconds since the Unix epoch. Typically this will come from the
  -- 'Signet.Unstable.Extra.Http.hWebhookTimestamp' header. For example
  -- @"981173106"@ for @2001-02-03T04:05:06Z@.
  Text.Text ->
  -- | The webhook's raw 'Payload.Payload'. Typically this will be a JSON
  -- object like @{"event_type":"ping",...}@.
  Text.Text ->
  -- | The webhook's 'Signatures.Signatures'. Typically this will come from the
  -- 'Signet.Unstable.Extra.Http.hWebhookSignature' header and look like
  -- @"v1_..."@ or @"v1a_..."@.
  Text.Text ->
  m Signature.Signature
verifyWebhookText v i t p s =
  verifyWebhookByteString
    (Text.encodeUtf8 v)
    (Text.encodeUtf8 i)
    (Text.encodeUtf8 t)
    (Text.encodeUtf8 p)
    (Text.encodeUtf8 s)

-- | Verifies a webhook with 'ByteString.ByteString' values. This is a
-- wrapper around 'verifyWebhook'. See 'verifyWebhookText' for a description of
-- the arguments.
verifyWebhookByteString ::
  (MonadIO.MonadIO m, Exception.MonadThrow m) =>
  -- | 'Verifier.Verifier'
  ByteString.ByteString ->
  -- | 'Id.Id'
  ByteString.ByteString ->
  -- | 'Timestamp.Timestamp'
  ByteString.ByteString ->
  -- | 'Payload.Payload'
  ByteString.ByteString ->
  -- | 'Signatures.Signatures'
  ByteString.ByteString ->
  m Signature.Signature
verifyWebhookByteString v i t p s = do
  verifier <- Either.throw $ parseVerifier v
  id_ <- Either.throw $ parseId i
  timestamp <- Either.throw $ parseTimestamp t
  let payload = Payload.MkPayload p
  let message =
        Message.MkMessage
          { Message.id_ = id_,
            Message.timestamp = timestamp,
            Message.payload = payload
          }
  (_, signatures) <- Either.throw $ parseSignatures s
  verifyWebhook verifier message signatures

-- | Verifies a webhook. This is a wrapper around 'verifyWebhookWith' that uses
-- 'typicalTolerance' and the current time.
--
-- Throws an exception if the webhook is invalid.
verifyWebhook ::
  (MonadIO.MonadIO m, Exception.MonadThrow m) =>
  Verifier.Verifier ->
  Message.Message ->
  Signatures.Signatures ->
  m Signature.Signature
verifyWebhook verifier message signatures = do
  now <- MonadIO.liftIO Time.getCurrentTime
  Either.throw $ verifyWebhookWith typicalTolerance now verifier message signatures

-- | Verifies a webhook. This is the lowest-level function that gives you the
-- most control. If you're looking for something that's easier to use and
-- assumes some reasonable defaults, consider 'verifyWebhook'.
verifyWebhookWith ::
  -- | Often 'typicalTolerance'.
  Tolerance.Tolerance ->
  -- | Usually 'Time.getCurrentTime'.
  Time.UTCTime ->
  -- | See 'parseVerifier'.
  Verifier.Verifier ->
  -- | See 'parseMessage'. Or 'Message.MkMessage' along with 'parseId',
  -- 'parseTimestamp', and 'Payload.MkPayload'.
  Message.Message ->
  -- | See 'parseSignatures'.
  Signatures.Signatures ->
  Either SignetException.SignetException Signature.Signature
verifyWebhookWith tolerance now verifier message signatures = do
  Bifunctor.first SignetException.ToleranceException
    . Tolerance.check tolerance now
    $ Message.timestamp message
  Bifunctor.first SignetException.VerificationException $
    Verifier.verify verifier message signatures

-- | Alias for 'Tolerance.typical'.
typicalTolerance :: Tolerance.Tolerance
typicalTolerance = Tolerance.typical

-- | Alias for 'Verifier.parse'.
parseVerifier :: ByteString.ByteString -> Either InvalidVerifier.InvalidVerifier Verifier.Verifier
parseVerifier = Verifier.parse

-- | Alias for 'Id.parse'.
parseId :: ByteString.ByteString -> Either InvalidId.InvalidId Id.Id
parseId = Id.parse

-- | Alias for 'Timestamp.parse'.
parseTimestamp :: ByteString.ByteString -> Either InvalidTimestamp.InvalidTimestamp Timestamp.Timestamp
parseTimestamp = Timestamp.parse

-- | Alias for 'Message.parse'.
parseMessage :: ByteString.ByteString -> Either InvalidMessage.InvalidMessage Message.Message
parseMessage = Message.parse

-- | Alias for 'Signatures.parse'.
parseSignatures ::
  ByteString.ByteString ->
  Either InvalidSignature.InvalidSignature ([UnknownSignature.UnknownSignature], Signatures.Signatures)
parseSignatures = Signatures.parse

-- | Alias for 'Verifier.Asymmetric'.
pattern AsymmetricVerifier :: PublicKey.PublicKey -> Verifier.Verifier
pattern AsymmetricVerifier publicKey = Verifier.Asymmetric publicKey

-- | Alias for 'Verifier.Symmetric'.
pattern SymmetricVerifier :: Secret.Secret -> Verifier.Verifier
pattern SymmetricVerifier secret = Verifier.Symmetric secret

{-# COMPLETE AsymmetricVerifier, SymmetricVerifier #-}

-- | Alias for 'Signer.sign'.
signWebhook :: Signer.Signer -> Message.Message -> Signature.Signature
signWebhook = Signer.sign

-- | Alias for 'Signer.parse'.
parseSigner :: ByteString.ByteString -> Either InvalidSigner.InvalidSigner Signer.Signer
parseSigner = Signer.parse

-- | Alias for 'Signer.Asymmetric'.
pattern AsymmetricSigner :: SecretKey.SecretKey -> Signer.Signer
pattern AsymmetricSigner secretKey = Signer.Asymmetric secretKey

-- | Alias for 'Signer.Symmetric'.
pattern SymmetricSigner :: Secret.Secret -> Signer.Signer
pattern SymmetricSigner secret = Signer.Symmetric secret

{-# COMPLETE AsymmetricSigner, SymmetricSigner #-}

-- | Alias for 'Signature.Asymmetric'.
pattern AsymmetricSignature :: Signature.Signature -> Signature.Signature
pattern AsymmetricSignature signature = signature

-- | Alias for 'Signature.Symmetric'.
pattern SymmetricSignature :: Signature.Signature -> Signature.Signature
pattern SymmetricSignature signature = signature

{-# COMPLETE AsymmetricSignature, SymmetricSignature #-}
