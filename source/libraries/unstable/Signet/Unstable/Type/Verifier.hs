module Signet.Unstable.Type.Verifier where

import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Signet.Unstable.Exception.InvalidVerifier as InvalidVerifier
import qualified Signet.Unstable.Exception.VerificationException as VerificationException
import qualified Signet.Unstable.Extra.Maybe as Maybe
import qualified Signet.Unstable.Type.AsymmetricSignature as AsymmetricSignature
import qualified Signet.Unstable.Type.Message as Message
import qualified Signet.Unstable.Type.PublicKey as PublicKey
import qualified Signet.Unstable.Type.Secret as Secret
import qualified Signet.Unstable.Type.Signature as Signature
import qualified Signet.Unstable.Type.Signatures as Signatures
import qualified Signet.Unstable.Type.Signer as Signer
import qualified Signet.Unstable.Type.SymmetricSignature as SymmetricSignature

data Verifier
  = Asymmetric PublicKey.PublicKey
  | Symmetric Secret.Secret
  deriving (Eq, Show)

parse :: ByteString.ByteString -> Either InvalidVerifier.InvalidVerifier Verifier
parse byteString = case PublicKey.parse byteString of
  Right publicKey -> Right $ Asymmetric publicKey
  Left _ -> case Secret.parse byteString of
    Right secret -> Right $ Symmetric secret
    Left _ -> Left $ InvalidVerifier.MkInvalidVerifier byteString

render :: Verifier -> ByteString.ByteString
render verifier = case verifier of
  Asymmetric publicKey -> PublicKey.render publicKey
  Symmetric secret -> Secret.render secret

verify ::
  Verifier ->
  Message.Message ->
  Signatures.Signatures ->
  Either VerificationException.VerificationException Signature.Signature
verify verifier message =
  case verifier of
    Asymmetric publicKey -> fmap Signature.Asymmetric . asymmetric publicKey message
    Symmetric secret -> fmap Signature.Symmetric . symmetric secret message

asymmetric ::
  PublicKey.PublicKey ->
  Message.Message ->
  Signatures.Signatures ->
  Either VerificationException.VerificationException AsymmetricSignature.AsymmetricSignature
asymmetric publicKey message =
  let toAsymmetric signature = case signature of
        Signature.Asymmetric asymmetricSignature -> Just asymmetricSignature
        Signature.Symmetric _ -> Nothing
      pk = PublicKey.unwrap publicKey
      bs = Message.render message
   in Maybe.note (VerificationException.MkVerificationException $ Message.id_ message)
        . List.find (Ed25519.verify pk bs . AsymmetricSignature.unwrap)
        . Maybe.mapMaybe toAsymmetric
        . Signatures.unwrap

symmetric ::
  Secret.Secret ->
  Message.Message ->
  Signatures.Signatures ->
  Either VerificationException.VerificationException SymmetricSignature.SymmetricSignature
symmetric secret message =
  let toSymmetric signature = case signature of
        Signature.Asymmetric _ -> Nothing
        Signature.Symmetric symmetricSignature -> Just symmetricSignature
      expected = Signer.symmetric secret message
   in Maybe.note (VerificationException.MkVerificationException $ Message.id_ message)
        . List.find (expected ==)
        . Maybe.mapMaybe toSymmetric
        . Signatures.unwrap
