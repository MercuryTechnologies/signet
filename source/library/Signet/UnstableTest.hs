module Signet.UnstableTest where

import qualified Data.ByteString.Char8 as Ascii
import qualified Data.Time as Time
import qualified Signet.Unstable as Signet
import qualified Signet.Unstable.Exception.SignetException as SignetException
import qualified Signet.Unstable.Exception.ToleranceException as ToleranceException
import qualified Signet.Unstable.Exception.VerificationException as VerificationException
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Extra.Tasty as Tasty
import qualified Signet.Unstable.Type.Message as Message
import qualified Signet.Unstable.Type.Signature as Signature
import qualified Signet.Unstable.Type.Signatures as Signatures
import qualified Signet.Unstable.Type.Tolerance as Tolerance
import qualified Signet.Unstable.Type.Verifier as Verifier
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable" $ do
  Tasty.describe "verifyWebhookText" $ do
    pure ()

  Tasty.describe "verifyWebhookByteString" $ do
    pure ()

  Tasty.describe "verifyWebhook" $ do
    pure ()

  Tasty.describe "verifyWebhookWith" $ do
    Tasty.it "succeeds with a valid symmetric signature" $ do
      let tolerance = Tolerance.typical
      let now = Time.UTCTime (Time.fromGregorian 1970 1 1) 0
      verifier <- Either.throw . Verifier.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      let byteString = Ascii.pack "v1,IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw="
      (_, signatures) <- Either.throw $ Signatures.parse byteString
      let result = Signet.verifyWebhookWith tolerance now verifier message signatures
      Right signature <- Either.throw $ Signature.parse byteString
      result @?= Right signature

    Tasty.it "fails with an invalid timestamp" $ do
      let tolerance = Tolerance.typical
      let now = Time.UTCTime (Time.fromGregorian 1970 1 1) 0
      verifier <- Either.throw . Verifier.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.301.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1,IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw="
      let result = Signet.verifyWebhookWith tolerance now verifier message signatures
      result @?= Left (SignetException.ToleranceException . ToleranceException.MkToleranceException $ Message.timestamp message)

    Tasty.it "fails with an invalid symmetric signature" $ do
      let tolerance = Tolerance.typical
      let now = Time.UTCTime (Time.fromGregorian 1970 1 1) 0
      verifier <- Either.throw . Verifier.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1,0000000000000000000000000000000000000000000="
      let result = Signet.verifyWebhookWith tolerance now verifier message signatures
      result @?= Left (SignetException.VerificationException . VerificationException.MkVerificationException $ Message.id_ message)
