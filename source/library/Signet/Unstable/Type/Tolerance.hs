module Signet.Unstable.Type.Tolerance where

import qualified Control.Monad as Monad
import qualified Data.Time as Time
import qualified Signet.Unstable.Exception.ToleranceException as ToleranceException
import qualified Signet.Unstable.Type.Timestamp as Timestamp

newtype Tolerance
  = MkTolerance Time.NominalDiffTime
  deriving (Eq, Show)

unwrap :: Tolerance -> Time.NominalDiffTime
unwrap (MkTolerance nominalDiffTime) = nominalDiffTime

typical :: Tolerance
typical = MkTolerance 300

check ::
  Tolerance ->
  Time.UTCTime ->
  Timestamp.Timestamp ->
  Either ToleranceException.ToleranceException ()
check tolerance utcTime timestamp = do
  let diff = Time.diffUTCTime utcTime $ Timestamp.unwrap timestamp
  let hi = unwrap tolerance
  let lo = negate hi
  Monad.when (lo > diff || diff > hi)
    . Left
    $ ToleranceException.MkToleranceException timestamp
