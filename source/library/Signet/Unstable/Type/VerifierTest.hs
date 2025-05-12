module Signet.Unstable.Type.VerifierTest where

import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidVerifier as InvalidVerifier
import qualified Signet.Unstable.Exception.VerificationException as VerificationException
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Extra.Tasty as Tasty
import qualified Signet.Unstable.Type.AsymmetricSignature as AsymmetricSignature
import qualified Signet.Unstable.Type.Message as Message
import qualified Signet.Unstable.Type.PublicKey as PublicKey
import qualified Signet.Unstable.Type.Secret as Secret
import qualified Signet.Unstable.Type.Signature as Signature
import qualified Signet.Unstable.Type.Signatures as Signatures
import qualified Signet.Unstable.Type.SymmetricSignature as SymmetricSignature
import qualified Signet.Unstable.Type.Verifier as Verifier
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Type.Verifier" $ do
  Tasty.describe "parse" $ do
    Tasty.it "succeeds with a valid public key" $ do
      let byteString = Ascii.pack "whpk_wuzPrKxPfWpJSsXgyg/MEoMGvjs5SjDO4ad6X4ZYqqg="
      let actual = Verifier.parse byteString
      publicKey <- Either.throw $ PublicKey.parse byteString
      actual @?= Right (Verifier.Asymmetric publicKey)

    Tasty.it "succeeds with a valid secret" $ do
      let byteString = Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      let actual = Verifier.parse byteString
      secret <- Either.throw $ Secret.parse byteString
      actual @?= Right (Verifier.Symmetric secret)

    Tasty.it "fails with invalid input" $ do
      let byteString = Ascii.pack "invalid"
      let actual = Verifier.parse byteString
      actual @?= Left (InvalidVerifier.MkInvalidVerifier byteString)

  Tasty.describe "render" $ do
    Tasty.it "works with a public key" $ do
      let byteString = Ascii.pack "whpk_wuzPrKxPfWpJSsXgyg/MEoMGvjs5SjDO4ad6X4ZYqqg="
      publicKey <- Either.throw . PublicKey.parse $ byteString
      Verifier.render (Verifier.Asymmetric publicKey) @?= byteString

    Tasty.it "works with a secret" $ do
      let byteString = Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      secret <- Either.throw . Secret.parse $ byteString
      Verifier.render (Verifier.Symmetric secret) @?= byteString

  Tasty.describe "verify" $ do
    Tasty.it "succeeds with a valid asymmetric signature" $ do
      verifier <- Either.throw . Verifier.parse $ Ascii.pack "whpk_wuzPrKxPfWpJSsXgyg/MEoMGvjs5SjDO4ad6X4ZYqqg="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1a,CV1O+PvrwXM42OMUX+tmm6bA3cS0tgLp0qo3YKuu0MGmBrsUhA0MHXF11HsEUJtPfTKs80WE7WUKVt9TueLDCQ=="
      let actual = Verifier.verify verifier message signatures
      Right signature <- Either.throw . Signature.parse $ Ascii.pack "v1a,CV1O+PvrwXM42OMUX+tmm6bA3cS0tgLp0qo3YKuu0MGmBrsUhA0MHXF11HsEUJtPfTKs80WE7WUKVt9TueLDCQ=="
      actual @?= Right signature

    Tasty.it "fails with an invalid asymmetric signature" $ do
      verifier <- Either.throw . Verifier.parse $ Ascii.pack "whpk_wuzPrKxPfWpJSsXgyg/MEoMGvjs5SjDO4ad6X4ZYqqg="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1a,00000000000000000000000000000000000000000000000000000000000000000000000000000000000000=="
      let actual = Verifier.verify verifier message signatures
      actual @?= Left (VerificationException.MkVerificationException $ Message.id_ message)

    Tasty.it "succeeds with a valid symmetric signature" $ do
      verifier <- Either.throw . Verifier.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1,IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw="
      let actual = Verifier.verify verifier message signatures
      Right signature <- Either.throw . Signature.parse $ Ascii.pack "v1,IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw="
      actual @?= Right signature

    Tasty.it "fails with an invalid symmetric signature" $ do
      verifier <- Either.throw . Verifier.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1,0000000000000000000000000000000000000000000="
      let actual = Verifier.verify verifier message signatures
      actual @?= Left (VerificationException.MkVerificationException $ Message.id_ message)

  Tasty.describe "asymmetric" $ do
    Tasty.it "succeeds with a valid signature" $ do
      publicKey <- Either.throw . PublicKey.parse $ Ascii.pack "whpk_wuzPrKxPfWpJSsXgyg/MEoMGvjs5SjDO4ad6X4ZYqqg="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1a,CV1O+PvrwXM42OMUX+tmm6bA3cS0tgLp0qo3YKuu0MGmBrsUhA0MHXF11HsEUJtPfTKs80WE7WUKVt9TueLDCQ=="
      let actual = Verifier.asymmetric publicKey message signatures
      asymmetricSignature <- Either.throw . AsymmetricSignature.parse $ Ascii.pack "CV1O+PvrwXM42OMUX+tmm6bA3cS0tgLp0qo3YKuu0MGmBrsUhA0MHXF11HsEUJtPfTKs80WE7WUKVt9TueLDCQ=="
      actual @?= Right asymmetricSignature

    Tasty.it "fails with an invalid signature" $ do
      publicKey <- Either.throw . PublicKey.parse $ Ascii.pack "whpk_wuzPrKxPfWpJSsXgyg/MEoMGvjs5SjDO4ad6X4ZYqqg="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1a,00000000000000000000000000000000000000000000000000000000000000000000000000000000000000=="
      let actual = Verifier.asymmetric publicKey message signatures
      actual @?= Left (VerificationException.MkVerificationException $ Message.id_ message)

  Tasty.describe "symmetric" $ do
    Tasty.it "succeeds with a valid signature" $ do
      secret <- Either.throw . Secret.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1,IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw="
      let actual = Verifier.symmetric secret message signatures
      symmetricSignature <- Either.throw . SymmetricSignature.parse $ Ascii.pack "IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw="
      actual @?= Right symmetricSignature

    Tasty.it "fails with an invalid signature" $ do
      secret <- Either.throw . Secret.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1,0000000000000000000000000000000000000000000="
      let actual = Verifier.symmetric secret message signatures
      actual @?= Left (VerificationException.MkVerificationException $ Message.id_ message)
