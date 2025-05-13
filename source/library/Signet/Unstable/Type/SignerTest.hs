module Signet.Unstable.Type.SignerTest where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidSigner as InvalidSigner
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Type.AsymmetricSignature as AsymmetricSignature
import qualified Signet.Unstable.Type.Message as Message
import qualified Signet.Unstable.Type.Secret as Secret
import qualified Signet.Unstable.Type.SecretKey as SecretKey
import qualified Signet.Unstable.Type.Signature as Signature
import qualified Signet.Unstable.Type.Signer as Signer
import qualified Signet.Unstable.Type.SymmetricSignature as SymmetricSignature
import qualified Signet.Unstable.Type.Test as Test

spec :: (Exception.MonadThrow io, Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable.Type.Signer" $ do
  Test.describe test "parse" $ do
    Test.it test "succeeds with a valid secret key" $ do
      let byteString = Ascii.pack "whsk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU="
      let actual = Signer.parse byteString
      secretKey <- Either.throw $ SecretKey.parse byteString
      Test.assertEq test actual (Right (Signer.Asymmetric secretKey))

    Test.it test "succeeds with a valid secret" $ do
      let byteString = Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      let actual = Signer.parse byteString
      secret <- Either.throw $ Secret.parse byteString
      Test.assertEq test actual (Right (Signer.Symmetric secret))

    Test.it test "fails with invalid input" $ do
      let byteString = Ascii.pack "invalid"
      let actual = Signer.parse byteString
      Test.assertEq test actual (Left (InvalidSigner.MkInvalidSigner byteString))

  Test.describe test "render" $ do
    Test.it test "works with a secret key" $ do
      let byteString = Ascii.pack "whsk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU="
      secretKey <- Either.throw . SecretKey.parse $ byteString
      Test.assertEq test (Signer.render (Signer.Asymmetric secretKey)) byteString

    Test.it test "works with a secret" $ do
      let byteString = Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      secret <- Either.throw . Secret.parse $ byteString
      Test.assertEq test (Signer.render (Signer.Symmetric secret)) byteString

  Test.describe test "sign" $ do
    Test.it test "works with asymmetric" $ do
      signer <- Either.throw . Signer.parse $ Ascii.pack "whsk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      let actual = Signer.sign signer message
      expected <- Either.throw =<< Either.throw (Signature.parse $ Ascii.pack "v1a,CV1O+PvrwXM42OMUX+tmm6bA3cS0tgLp0qo3YKuu0MGmBrsUhA0MHXF11HsEUJtPfTKs80WE7WUKVt9TueLDCQ==")
      Test.assertEq test actual expected

    Test.it test "works with symmetric" $ do
      signer <- Either.throw . Signer.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      let actual = Signer.sign signer message
      expected <- Either.throw =<< Either.throw (Signature.parse $ Ascii.pack "v1,IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw=")
      Test.assertEq test actual expected

  Test.describe test "asymmetric" $ do
    Test.it test "creates correct asymmetric signature" $ do
      secretKey <- Either.throw . SecretKey.parse $ Ascii.pack "whsk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      let actual = Signer.asymmetric secretKey message
      expected <- Either.throw . AsymmetricSignature.parse $ Ascii.pack "CV1O+PvrwXM42OMUX+tmm6bA3cS0tgLp0qo3YKuu0MGmBrsUhA0MHXF11HsEUJtPfTKs80WE7WUKVt9TueLDCQ=="
      Test.assertEq test actual expected

  Test.describe test "symmetric" $ do
    Test.it test "creates correct symmetric signature" $ do
      secret <- Either.throw . Secret.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      let actual = Signer.symmetric secret message
      expected <- Either.throw . SymmetricSignature.parse $ Ascii.pack "IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw="
      Test.assertEq test actual expected
