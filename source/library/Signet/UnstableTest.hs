module Signet.UnstableTest where

import qualified Control.Monad.Catch as Exception
import qualified Data.ByteString.Char8 as Ascii
import qualified Data.Time as Time
import qualified Signet.Unstable as Signet
import qualified Signet.Unstable.Exception.SignetException as SignetException
import qualified Signet.Unstable.Exception.ToleranceException as ToleranceException
import qualified Signet.Unstable.Exception.VerificationException as VerificationException
import qualified Signet.Unstable.Extra.Either as Either
import qualified Signet.Unstable.Type.Message as Message
import qualified Signet.Unstable.Type.Signature as Signature
import qualified Signet.Unstable.Type.Signatures as Signatures
import qualified Signet.Unstable.Type.Test as Test
import qualified Signet.Unstable.Type.Tolerance as Tolerance
import qualified Signet.Unstable.Type.Verifier as Verifier

spec :: (Exception.MonadThrow io, Monad tree) => Test.Test io tree -> tree ()
spec test = Test.describe test "Signet.Unstable" $ do
  Test.describe test "verifyWebhookText" $ do
    pure ()

  Test.describe test "verifyWebhookByteString" $ do
    pure ()

  Test.describe test "verifyWebhook" $ do
    pure ()

  Test.describe test "verifyWebhookWith" $ do
    Test.it test "succeeds with a valid symmetric signature" $ do
      let tolerance = Tolerance.typical
      let now = Time.UTCTime (Time.fromGregorian 1970 1 1) 0
      verifier <- Either.throw . Verifier.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      let byteString = Ascii.pack "v1,IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw="
      (_, signatures) <- Either.throw $ Signatures.parse byteString
      let result = Signet.verifyWebhookWith tolerance now verifier message signatures
      signature <- Either.throw =<< Either.throw (Signature.parse byteString)
      Test.assertEq test result (Right signature)

    Test.it test "fails with an invalid timestamp" $ do
      let tolerance = Tolerance.typical
      let now = Time.UTCTime (Time.fromGregorian 1970 1 1) 0
      verifier <- Either.throw . Verifier.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.301.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1,IywpE5NXy+JdAScgR7j5Pt59GjmazD7iJuVsQoRZFyw="
      let result = Signet.verifyWebhookWith tolerance now verifier message signatures
      Test.assertEq test result (Left (SignetException.ToleranceException . ToleranceException.MkToleranceException $ Message.timestamp message))

    Test.it test "fails with an invalid symmetric signature" $ do
      let tolerance = Tolerance.typical
      let now = Time.UTCTime (Time.fromGregorian 1970 1 1) 0
      verifier <- Either.throw . Verifier.parse $ Ascii.pack "whsec_bXlzZWNyZXRrZXkxMjM0NQ=="
      message <- Either.throw . Message.parse $ Ascii.pack "i.0.Hello, world!"
      (_, signatures) <- Either.throw . Signatures.parse $ Ascii.pack "v1,0000000000000000000000000000000000000000000="
      let result = Signet.verifyWebhookWith tolerance now verifier message signatures
      Test.assertEq test result (Left (SignetException.VerificationException . VerificationException.MkVerificationException $ Message.id_ message))
