module Signet.Unstable.Type.VerifierTest where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString.Char8 as Ascii
import qualified Heck as Test
import qualified Signet.Unstable.Exception.InvalidVerifier as InvalidVerifier
import qualified Signet.Unstable.Exception.VerificationException as VerificationException
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Type.AsymmetricSignature as AsymmetricSignature
import qualified Signet.Unstable.Type.Message as Message
import qualified Signet.Unstable.Type.PublicKey as PublicKey
import qualified Signet.Unstable.Type.Secret as Secret
import qualified Signet.Unstable.Type.Signature as Signature
import qualified Signet.Unstable.Type.Signatures as Signatures
import qualified Signet.Unstable.Type.SymmetricSignature as SymmetricSignature
import qualified Signet.Unstable.Type.Verifier as Verifier

spec :: (Exception.MonadThrow io, Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Type.Verifier" $ do
  Test.describe test "parse" $ do
    Test.it test "succeeds with a valid public key" $ do
      let byteString = Ascii.pack "whpk_wuzPrKxPfWpJSsXgyg/MEoMGvjs5SjDO4ad6X4ZYqqg="
      let actual = Verifier.parse byteString
      publicKey <- Either.throw $ PublicKey.parse byteString
      Test.assertEq test actual (Right (Verifier.Asymmetric publicKey))

    Test.it test "succeeds with a valid secret" $ do
      let byteString = Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      let actual = Verifier.parse byteString
      secret <- Either.throw $ Secret.parse byteString
      Test.assertEq test actual (Right (Verifier.Symmetric secret))

    Test.it test "fails with invalid input" $ do
      let byteString = Ascii.pack "invalid"
      let actual = Verifier.parse byteString
      Test.assertEq test actual (Left (InvalidVerifier.MkInvalidVerifier byteString))

  Test.describe test "render" $ do
    Test.it test "works with a public key" $ do
      let byteString = Ascii.pack "whpk_wuzPrKxPfWpJSsXgyg/MEoMGvjs5SjDO4ad6X4ZYqqg="
      publicKey <- Either.throw . PublicKey.parse $ byteString
      Test.assertEq test (Verifier.render (Verifier.Asymmetric publicKey)) byteString

    Test.it test "works with a secret" $ do
      let byteString = Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      secret <- Either.throw . Secret.parse $ byteString
      Test.assertEq test (Verifier.render (Verifier.Symmetric secret)) byteString

  Test.describe test "verify" $ do
    Test.it test "succeeds with a valid asymmetric signature" $ do
      verifier <- Either.throw . Verifier.parse $ Ascii.pack "whpk_wuzPrKxPfWpJSsXgyg/MEoMGvjs5SjDO4ad6X4ZYqqg="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1a,CV1O+PvrwXM42OMUX+tmm6bA3cS0tgLp0qo3YKuu0MGmBrsUhA0MHXF11HsEUJtPfTKs80WE7WUKVt9TueLDCQ=="
      let actual = Verifier.verify verifier message signatures
      signature <- Either.throw =<< Either.throw (Signature.parse $ Ascii.pack "v1a,CV1O+PvrwXM42OMUX+tmm6bA3cS0tgLp0qo3YKuu0MGmBrsUhA0MHXF11HsEUJtPfTKs80WE7WUKVt9TueLDCQ==")
      Test.assertEq test actual (Right signature)

    Test.it test "fails with an invalid asymmetric signature" $ do
      verifier <- Either.throw . Verifier.parse $ Ascii.pack "whpk_wuzPrKxPfWpJSsXgyg/MEoMGvjs5SjDO4ad6X4ZYqqg="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1a,00000000000000000000000000000000000000000000000000000000000000000000000000000000000000=="
      let actual = Verifier.verify verifier message signatures
      Test.assertEq test actual (Left (VerificationException.MkVerificationException $ Message.id_ message))

    Test.it test "succeeds with a valid symmetric signature" $ do
      verifier <- Either.throw . Verifier.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1,IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw="
      let actual = Verifier.verify verifier message signatures
      signature <- Either.throw =<< Either.throw (Signature.parse $ Ascii.pack "v1,IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw=")
      Test.assertEq test actual (Right signature)

    Test.it test "fails with an invalid symmetric signature" $ do
      verifier <- Either.throw . Verifier.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1,0000000000000000000000000000000000000000000="
      let actual = Verifier.verify verifier message signatures
      Test.assertEq test actual (Left (VerificationException.MkVerificationException $ Message.id_ message))

  Test.describe test "asymmetric" $ do
    Test.it test "succeeds with a valid signature" $ do
      publicKey <- Either.throw . PublicKey.parse $ Ascii.pack "whpk_wuzPrKxPfWpJSsXgyg/MEoMGvjs5SjDO4ad6X4ZYqqg="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1a,CV1O+PvrwXM42OMUX+tmm6bA3cS0tgLp0qo3YKuu0MGmBrsUhA0MHXF11HsEUJtPfTKs80WE7WUKVt9TueLDCQ=="
      let actual = Verifier.asymmetric publicKey message signatures
      asymmetricSignature <- Either.throw . AsymmetricSignature.parse $ Ascii.pack "CV1O+PvrwXM42OMUX+tmm6bA3cS0tgLp0qo3YKuu0MGmBrsUhA0MHXF11HsEUJtPfTKs80WE7WUKVt9TueLDCQ=="
      Test.assertEq test actual (Right asymmetricSignature)

    Test.it test "fails with an invalid signature" $ do
      publicKey <- Either.throw . PublicKey.parse $ Ascii.pack "whpk_wuzPrKxPfWpJSsXgyg/MEoMGvjs5SjDO4ad6X4ZYqqg="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1a,00000000000000000000000000000000000000000000000000000000000000000000000000000000000000=="
      let actual = Verifier.asymmetric publicKey message signatures
      Test.assertEq test actual (Left (VerificationException.MkVerificationException $ Message.id_ message))

  Test.describe test "symmetric" $ do
    Test.it test "succeeds with a valid signature" $ do
      secret <- Either.throw . Secret.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1,IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw="
      let actual = Verifier.symmetric secret message signatures
      symmetricSignature <- Either.throw . SymmetricSignature.parse $ Ascii.pack "IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw="
      Test.assertEq test actual (Right symmetricSignature)

    Test.it test "fails with an invalid signature" $ do
      secret <- Either.throw . Secret.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1,0000000000000000000000000000000000000000000="
      let actual = Verifier.symmetric secret message signatures
      Test.assertEq test actual (Left (VerificationException.MkVerificationException $ Message.id_ message))
