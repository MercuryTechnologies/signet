module Signet.Unstable.Type.IdTest where

import qualified Data.ByteString.Char8 as Ascii
import qualified Signet.Unstable.Exception.InvalidId as InvalidId
import qualified Signet.Unstable.Extra.Tasty as Tasty
import qualified Signet.Unstable.Type.Id as Id
import Test.Tasty.HUnit ((@?=))

spec :: Tasty.Spec
spec = Tasty.describe "Signet.Unstable.Type.Id" $ do
  Tasty.describe "parse" $ do
    Tasty.it "fails with input containing separator" $ do
      let byteString = Ascii.pack "invalid.id"
      let result = Id.parse byteString
      result @?= Left (InvalidId.MkInvalidId byteString)

    Tasty.it "succeeds with valid input" $ do
      let byteString = Ascii.pack "valid-id"
      let result = Id.parse byteString
      result @?= Right (Id.MkId byteString)

  Tasty.describe "render" $ do
    Tasty.it "returns the original ByteString" $ do
      let byteString = Ascii.pack "valid-id"
      let id_ = Id.MkId byteString
      Id.render id_ @?= byteString
