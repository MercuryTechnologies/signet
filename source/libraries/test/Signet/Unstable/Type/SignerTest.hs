module Signet.Unstable.Type.SignerTest where

import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidSigner as InvalidSigner
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Extra.Tasty as Tasty
import qualified Signet.Unstable.Type.AsymmetricSignature as AsymmetricSignature
import qualified Signet.Unstable.Type.Message as Message
import qualified Signet.Unstable.Type.Secret as Secret
import qualified Signet.Unstable.Type.SecretKey as SecretKey
import qualified Signet.Unstable.Type.Signature as Signature
import qualified Signet.Unstable.Type.Signer as Signer
import qualified Signet.Unstable.Type.SymmetricSignature as SymmetricSignature
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Type.Signer" $ do
  Tasty.describe "parse" $ do
    Tasty.it "succeeds with a valid secret key" $ do
      let byteString = Ascii.pack "whsk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU="
      let actual = Signer.parse byteString
      secretKey <- Either.throw $ SecretKey.parse byteString
      actual @?= Right (Signer.Asymmetric secretKey)

    Tasty.it "succeeds with a valid secret" $ do
      let byteString = Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      let actual = Signer.parse byteString
      secret <- Either.throw $ Secret.parse byteString
      actual @?= Right (Signer.Symmetric secret)

    Tasty.it "fails with invalid input" $ do
      let byteString = Ascii.pack "invalid"
      let actual = Signer.parse byteString
      actual @?= Left (InvalidSigner.MkInvalidSigner byteString)

  Tasty.describe "render" $ do
    Tasty.it "works with a secret key" $ do
      let byteString = Ascii.pack "whsk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU="
      secretKey <- Either.throw . SecretKey.parse $ byteString
      Signer.render (Signer.Asymmetric secretKey) @?= byteString

    Tasty.it "works with a secret" $ do
      let byteString = Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      secret <- Either.throw . Secret.parse $ byteString
      Signer.render (Signer.Symmetric secret) @?= byteString

  Tasty.describe "sign" $ do
    Tasty.it "works with asymmetric" $ do
      signer <- Either.throw . Signer.parse $ Ascii.pack "whsk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      let actual = Signer.sign signer message
      Right expected <- Either.throw . Signature.parse $ Ascii.pack "v1a,CV1O+PvrwXM42OMUX+tmm6bA3cS0tgLp0qo3YKuu0MGmBrsUhA0MHXF11HsEUJtPfTKs80WE7WUKVt9TueLDCQ=="
      actual @?= expected

    Tasty.it "works with symmetric" $ do
      signer <- Either.throw . Signer.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      let actual = Signer.sign signer message
      Right expected <- Either.throw . Signature.parse $ Ascii.pack "v1,IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw="
      actual @?= expected

  Tasty.describe "asymmetric" $ do
    Tasty.it "creates correct asymmetric signature" $ do
      secretKey <- Either.throw . SecretKey.parse $ Ascii.pack "whsk_QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVowMTIzNDU="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      let actual = Signer.asymmetric secretKey message
      expected <- Either.throw . AsymmetricSignature.parse $ Ascii.pack "CV1O+PvrwXM42OMUX+tmm6bA3cS0tgLp0qo3YKuu0MGmBrsUhA0MHXF11HsEUJtPfTKs80WE7WUKVt9TueLDCQ=="
      actual @?= expected

  Tasty.describe "symmetric" $ do
    Tasty.it "creates correct symmetric signature" $ do
      secret <- Either.throw . Secret.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      let actual = Signer.symmetric secret message
      expected <- Either.throw . SymmetricSignature.parse $ Ascii.pack "IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw="
      actual @?= expected
