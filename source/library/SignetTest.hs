module SignetTest where

import qualified Control.Monad.Trans.Writer as Writer
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
import qualified Signet.Unstable.Type.TimestampTest
import qualified Signet.Unstable.Type.ToleranceTest
import qualified Signet.Unstable.Type.UnknownSignatureTest
import qualified Signet.Unstable.Type.VerifierTest
import qualified Signet.UnstableTest
import qualified Test.Tasty as Tasty

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "signet" . Writer.execWriter $ do
  Signet.Unstable.Exception.InvalidAsymmetricSignatureTest.spec
  Signet.Unstable.Exception.InvalidIdTest.spec
  Signet.Unstable.Exception.InvalidMessageTest.spec
  Signet.Unstable.Exception.InvalidPublicKeyTest.spec
  Signet.Unstable.Exception.InvalidSecretKeyTest.spec
  Signet.Unstable.Exception.InvalidSecretTest.spec
  Signet.Unstable.Exception.InvalidSignatureTest.spec
  Signet.Unstable.Exception.InvalidSignerTest.spec
  Signet.Unstable.Exception.InvalidSymmetricSignatureTest.spec
  Signet.Unstable.Exception.InvalidTimestampTest.spec
  Signet.Unstable.Exception.InvalidVerifierTest.spec
  Signet.Unstable.Exception.SignetExceptionTest.spec
  Signet.Unstable.Exception.ToleranceExceptionTest.spec
  Signet.Unstable.Exception.VerificationExceptionTest.spec
  Signet.Unstable.Extra.EitherTest.spec
  Signet.Unstable.Extra.HttpTest.spec
  Signet.Unstable.Extra.MaybeTest.spec
  Signet.Unstable.Type.AsymmetricSignatureTest.spec
  Signet.Unstable.Type.IdTest.spec
  Signet.Unstable.Type.MessageTest.spec
  Signet.Unstable.Type.PayloadTest.spec
  Signet.Unstable.Type.PublicKeyTest.spec
  Signet.Unstable.Type.SecretKeyTest.spec
  Signet.Unstable.Type.SecretTest.spec
  Signet.Unstable.Type.SignaturesTest.spec
  Signet.Unstable.Type.SignatureTest.spec
  Signet.Unstable.Type.SignerTest.spec
  Signet.Unstable.Type.SymmetricSignatureTest.spec
  Signet.Unstable.Type.TimestampTest.spec
  Signet.Unstable.Type.ToleranceTest.spec
  Signet.Unstable.Type.UnknownSignatureTest.spec
  Signet.Unstable.Type.VerifierTest.spec
  Signet.UnstableTest.spec
