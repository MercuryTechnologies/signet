module Signet.Unstable.Type.Signer where

import qualified Crypto.MAC.HMAC as Hmac
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteString as ByteString
import qualified Signet.Unstable.Exception.InvalidSigner as InvalidSigner
import qualified Signet.Unstable.Type.AsymmetricSignature as AsymmetricSignature
import qualified Signet.Unstable.Type.Message as Message
import qualified Signet.Unstable.Type.Secret as Secret
import qualified Signet.Unstable.Type.SecretKey as SecretKey
import qualified Signet.Unstable.Type.Signature as Signature
import qualified Signet.Unstable.Type.SymmetricSignature as SymmetricSignature

data Signer
  = Asymmetric SecretKey.SecretKey
  | Symmetric Secret.Secret
  deriving (Eq, Show)

parse :: ByteString.ByteString -> Either InvalidSigner.InvalidSigner Signer
parse byteString = case SecretKey.parse byteString of
  Right secretKey -> Right $ Asymmetric secretKey
  Left _ -> case Secret.parse byteString of
    Right secret -> Right $ Symmetric secret
    Left _ -> Left $ InvalidSigner.MkInvalidSigner byteString

render :: Signer -> ByteString.ByteString
render signer = case signer of
  Asymmetric secretKey -> SecretKey.render secretKey
  Symmetric secret -> Secret.render secret

sign :: Signer -> Message.Message -> Signature.Signature
sign signer = case signer of
  Asymmetric secretKey -> Signature.Asymmetric . asymmetric secretKey
  Symmetric secret -> Signature.Symmetric . symmetric secret

asymmetric :: SecretKey.SecretKey -> Message.Message -> AsymmetricSignature.AsymmetricSignature
asymmetric secretKey =
  let sk = SecretKey.unwrap secretKey
      pk = Ed25519.toPublic sk
   in AsymmetricSignature.MkAsymmetricSignature
        . Ed25519.sign sk pk
        . Message.render

symmetric :: Secret.Secret -> Message.Message -> SymmetricSignature.SymmetricSignature
symmetric secret =
  SymmetricSignature.MkSymmetricSignature
    . Hmac.hmacGetDigest
    . Hmac.hmac (Secret.unwrap secret)
    . Message.render
