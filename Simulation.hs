{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Simulation where

import Clash.Prelude hiding (zip, undefined)
import qualified Clash.Explicit.Prelude as CE
import Prelude hiding ((!!), replicate, head)
import qualified Prelude as P
import Protocols
import Protocols.Df hiding (simulate)
import Debug.Trace
import Data.Proxy
import Data.Coerce
import           Data.Bool (bool)
import qualified Data.Maybe as Maybe

import qualified Protocols.Hedgehog as H
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List ((\\))

import Test.Tasty
-- import Test.Tasty.TH (testGroupGenerator)
import Test.Tasty.Hedgehog.Extra (testProperty)




staller :: Circuit (Df System Int) (Df System Int)
staller = stallC (def {resetCycles = 0}) ((StallTransparently,[3,3..]):>Nil)


sigs inp = rhs_data
  where
    (lhs_data, rhs_ack) = unbundle inp
    (lhs_ack, rhs_data) = (toSignals staller) (lhs_data, rhs_ack)

