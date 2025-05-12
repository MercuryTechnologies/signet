module Signet.Unstable.Exception.InvalidSignature where

import qualified Control.Monad.Catch as Exception
import qualified Signet.Unstable.Exception.InvalidAsymmetricSignature as InvalidAsymmetricSignature
import qualified Signet.Unstable.Exception.InvalidSymmetricSignature as InvalidSymmetricSignature

data InvalidSignature
  = InvalidAsymmetricSignature InvalidAsymmetricSignature.InvalidAsymmetricSignature
  | InvalidSymmetricSignature InvalidSymmetricSignature.InvalidSymmetricSignature
  deriving (Eq, Show)

instance Exception.Exception InvalidSignature where
  displayException invalidSignature =
    "invalid signature: " <> case invalidSignature of
      InvalidAsymmetricSignature invalidAsymmetricSignature ->
        Exception.displayException invalidAsymmetricSignature
      InvalidSymmetricSignature invalidSymmetricSignature ->
        Exception.displayException invalidSymmetricSignature
