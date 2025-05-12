module Signet.Unstable.Extra.Tasty where

import qualified Control.Monad.Trans.Writer as Writer
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty

type Spec = Writer.Writer [Tasty.TestTree] ()

describe :: Tasty.TestName -> Spec -> Spec
describe testName = Writer.tell . pure . Tasty.testGroup testName . Writer.execWriter

it :: Tasty.TestName -> Tasty.Assertion -> Spec
it testName = Writer.tell . pure . Tasty.testCase testName
