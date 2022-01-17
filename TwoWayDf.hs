{-|
Defines data structures and operators to create a Dataflow protocol supporting
two-way data transfers.

-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Protocols.TwoWayDf where

import           Prelude hiding
  ((!!), map, zip, zipWith, filter, fst, snd, either, const, pure)

import Clash.Prelude
import qualified Clash.Prelude as C
import qualified Prelude as P
import           Data.Kind (Type)
import Protocols
import Protocols.Df
import Protocols.Internal
import qualified Data.Maybe as Maybe
import           Data.Proxy

data TwoWayDf (dom :: C.Domain) (l2r :: Type) (r2l :: Type)

data LeftOnly l r = Left l

instance Protocol (TwoWayDf dom l2r r2l) where
  type Fwd (TwoWayDf dom l2r r2l) = Signal dom (LeftOnly (Data l2r) r2l, Ack) -- l2r data, r2l ack
  type Bwd (TwoWayDf dom l2r r2l) = Signal dom (Data r2l, Ack) -- r2l data, l2r ack

-- instance (C.KnownDomain dom,
--   C.NFDataX l2r, C.ShowX l2r, Show l2r,
--   C.NFDataX r2l, C.ShowX r2l, Show r2l) =>
--   Simulate (TwoWayDf dom l2r r2l) where

--   type SimulateType (TwoWayDf dom l2r r2l) = [Data l2r]
--   type ExpectType (TwoWayDf dom l2r r2l) = [l2r]
--   type SimulateChannels (TwoWayDf dom l2r r2l) = 1

--   toSimulateType Proxy = P.map Data
--   fromSimulateType Proxy = Maybe.mapMaybe dataToMaybe

--   driveC = Circuit go
