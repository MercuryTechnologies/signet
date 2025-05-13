module SignetTest where

import qualified Control.Monad.Catch as Exception
import qualified Signet.Unstable.Exception.InvalidAsymmetricSignatureTest
import qualified Signet.Unstable.Exception.InvalidIdTest
import qualified Signet.Unstable.Exception.InvalidMessageTest
import qualified Signet.Unstable.Exception.InvalidPublicKeyTest
import qualified Signet.Unstable.Exception.InvalidSecretKeyTest
import qualified Signet.Unstable.Exception.InvalidSecretTest
import qualified Signet.Unstable.Exception.InvalidSignatureTest
import qualified Signet.Unstable.Exception.InvalidSignerTest
import qualified Signet.Unstable.Exception.InvalidSymmetricSignatureTest
import qualified Signet.Unstable.Exception.InvalidTimestampTest
import qualified Signet.Unstable.Exception.InvalidVerifierTest
import qualified Signet.Unstable.Exception.SignetExceptionTest
import qualified Signet.Unstable.Exception.ToleranceExceptionTest
import qualified Signet.Unstable.Exception.UnknownSignatureTest
import qualified Signet.Unstable.Exception.VerificationExceptionTest
import qualified Signet.Unstable.Extra.EitherTest
import qualified Signet.Unstable.Extra.HttpTest
import qualified Signet.Unstable.Extra.MaybeTest
import qualified Signet.Unstable.Type.AsymmetricSignatureTest
import qualified Signet.Unstable.Type.IdTest
import qualified Signet.Unstable.Type.MessageTest
import qualified Signet.Unstable.Type.PayloadTest
import qualified Signet.Unstable.Type.PublicKeyTest
import qualified Signet.Unstable.Type.SecretKeyTest
import qualified Signet.Unstable.Type.SecretTest
import qualified Signet.Unstable.Type.SignatureTest
import qualified Signet.Unstable.Type.SignaturesTest
import qualified Signet.Unstable.Type.SignerTest
import qualified Signet.Unstable.Type.SymmetricSignatureTest
import qualified Signet.Unstable.Type.Test as Test
import qualified Signet.Unstable.Type.TimestampTest
import qualified Signet.Unstable.Type.ToleranceTest
import qualified Signet.Unstable.Type.VerifierTest
import qualified Signet.UnstableTest

spec :: (Exception.MonadCatch io, Monad tree) => Test.Test io tree -> tree ()
spec test = do
  Signet.Unstable.Exception.InvalidAsymmetricSignatureTest.spec test
  Signet.Unstable.Exception.InvalidIdTest.spec test
  Signet.Unstable.Exception.InvalidMessageTest.spec test
  Signet.Unstable.Exception.InvalidPublicKeyTest.spec test
  Signet.Unstable.Exception.InvalidSecretKeyTest.spec test
  Signet.Unstable.Exception.InvalidSecretTest.spec test
  Signet.Unstable.Exception.InvalidSignatureTest.spec test
  Signet.Unstable.Exception.InvalidSignerTest.spec test
  Signet.Unstable.Exception.InvalidSymmetricSignatureTest.spec test
  Signet.Unstable.Exception.InvalidTimestampTest.spec test
  Signet.Unstable.Exception.InvalidVerifierTest.spec test
  Signet.Unstable.Exception.SignetExceptionTest.spec test
  Signet.Unstable.Exception.ToleranceExceptionTest.spec test
  Signet.Unstable.Exception.VerificationExceptionTest.spec test
  Signet.Unstable.Extra.EitherTest.spec test
  Signet.Unstable.Extra.HttpTest.spec test
  Signet.Unstable.Extra.MaybeTest.spec test
  Signet.Unstable.Type.AsymmetricSignatureTest.spec test
  Signet.Unstable.Type.IdTest.spec test
  Signet.Unstable.Type.MessageTest.spec test
  Signet.Unstable.Type.PayloadTest.spec test
  Signet.Unstable.Type.PublicKeyTest.spec test
  Signet.Unstable.Type.SecretKeyTest.spec test
  Signet.Unstable.Type.SecretTest.spec test
  Signet.Unstable.Type.SignaturesTest.spec test
  Signet.Unstable.Type.SignatureTest.spec test
  Signet.Unstable.Type.SignerTest.spec test
  Signet.Unstable.Type.SymmetricSignatureTest.spec test
  Signet.Unstable.Type.TimestampTest.spec test
  Signet.Unstable.Type.ToleranceTest.spec test
  Signet.Unstable.Exception.UnknownSignatureTest.spec test
  Signet.Unstable.Type.VerifierTest.spec test
  Signet.UnstableTest.spec test
