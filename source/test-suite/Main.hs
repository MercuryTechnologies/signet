import qualified Control.Monad.Trans.Writer as Writer
import qualified Signet.Unstable.Type.Test as Test
import qualified SignetTest
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Unit

main :: IO ()
main = Tasty.defaultMain testTree

testTree :: Tasty.TestTree
testTree = Tasty.testGroup "signet" . Writer.execWriter $ SignetTest.spec tasty

tasty :: Test.Test (Writer.Writer [Tasty.TestTree])
tasty =
  Test.MkTest
    { Test.assertFailure = Unit.assertFailure,
      Test.describe = \x -> Writer.tell . pure . Tasty.testGroup x . Writer.execWriter,
      Test.it = \x -> Writer.tell . pure . Unit.testCase x
    }
