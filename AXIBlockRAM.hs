{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module AXIBlockRAM where

import Prelude hiding ((!!))

import Protocols
import Clash.Prelude hiding (map, undefined, take, zip)
import qualified Protocols.Axi4.Common as Axi4
import qualified Protocols.Axi4.ReadAddress as ReadAddress
import qualified Protocols.Axi4.ReadData as ReadData
import qualified Protocols.Axi4.WriteAddress as WriteAddress
import qualified Protocols.Axi4.WriteData as WriteData
import qualified Protocols.Axi4.WriteResponse as WriteResponse


import Data.Bifunctor

-------------------------
-- Read channel types ---
-------------------------

type SimpleRD dom = ReadData.Axi4ReadData dom
  'Axi4.KeepResponse
  ('Axi4.IdWidth 1) ()
  (Unsigned 8)

type SimpleRA dom = ReadAddress.Axi4ReadAddress dom
  ('Axi4.KeepBurst)
  ('Axi4.KeepSize)
  ('Axi4.LengthWidth 8) -- Is dit niet in het protocol gedefinieerd op [7:0] ?
  ('Axi4.IdWidth 1)
  ('Axi4.AddrWidth 4)
  ('Axi4.NoRegion)
  ('Axi4.KeepBurstLength)
  ('Axi4.NoLock)
  ('Axi4.NoCache)
  ('Axi4.NoPermissions)
  ('Axi4.NoQos)
  ()

type SimpleRA_S2M = ReadAddress.S2M_ReadAddress
type SimpleRA_M2S = ReadAddress.M2S_ReadAddress
  'Axi4.KeepBurst
  'Axi4.KeepSize
  ('Axi4.LengthWidth 8)
  ('Axi4.IdWidth 1)
  ('Axi4.AddrWidth 4)
  'Axi4.NoRegion
  'Axi4.KeepBurstLength
  'Axi4.NoLock
  'Axi4.NoCache
  'Axi4.NoPermissions
  'Axi4.NoQos
  ()

defaultSimpleRA_M2S :: SimpleRA_M2S
defaultSimpleRA_M2S = ReadAddress.M2S_ReadAddress {
    _arid = 0,
    _araddr = 0,
    _arregion = (),
    _arlen = 0,
    _arsize = Axi4.Bs8,
    _arburst = Axi4.BmFixed,
    _arlock = (),
    _arcache = (),
    _arprot = (),
    _arqos = (),
    _aruser = ()
  }


type SimpleRD_M2S = ReadData.M2S_ReadData
type SimpleRD_S2M = ReadData.S2M_ReadData
  'Axi4.KeepResponse
  ('Axi4.IdWidth 1)
  ()
  (Unsigned 8)

defaultSimpleRD_S2M :: SimpleRD_S2M
defaultSimpleRD_S2M = ReadData.S2M_ReadData {
  _rid = 0,
  _rdata = 0,
  _rresp = Axi4.ROkay,
  _rlast = False,
  _ruser = ()
}

---------------------------
--- Write channel types ---
---------------------------

type SimpleWA dom = WriteAddress.Axi4WriteAddress dom
  ('Axi4.KeepBurst)
  ('Axi4.KeepSize)
  ('Axi4.LengthWidth 8)
  ('Axi4.IdWidth 1)
  ('Axi4.AddrWidth 4)
  ('Axi4.NoRegion)
  ('Axi4.KeepBurstLength)
  ('Axi4.NoLock)
  ('Axi4.NoCache)
  ('Axi4.NoPermissions)
  ('Axi4.NoQos)
  ()

type SimpleWD dom = WriteData.Axi4WriteData dom
  ('Axi4.NoStrobe)
  (1)
  ()

type SimpleWR dom = WriteResponse.Axi4WriteResponse dom
  ('Axi4.KeepResponse)
  ('Axi4.IdWidth 1)
  ()

type SimpleWA_S2M = WriteAddress.S2M_WriteAddress
type SimpleWA_M2S = WriteAddress.M2S_WriteAddress
  ('Axi4.KeepBurst)
  ('Axi4.KeepSize)
  ('Axi4.LengthWidth 8)
  ('Axi4.IdWidth 1)
  ('Axi4.AddrWidth 4)
  ('Axi4.NoRegion)
  ('Axi4.KeepBurstLength)
  ('Axi4.NoLock)
  ('Axi4.NoCache)
  ('Axi4.NoPermissions)
  ('Axi4.NoQos)
  ()

defaultSimpleWA_M2S :: SimpleWA_M2S
defaultSimpleWA_M2S = WriteAddress.M2S_WriteAddress {
    _awid = 0,
    _awaddr = 0,
    _awregion = (),
    _awlen = 0,
    _awsize = Axi4.Bs8,
    _awburst = Axi4.BmFixed,
    _awlock = (),
    _awcache = (),
    _awprot = (),
    _awqos = (),
    _awuser = ()
  }

type SimpleWD_S2M = WriteData.S2M_WriteData
type SimpleWD_M2S = WriteData.M2S_WriteData
  ('Axi4.NoStrobe)
  (1)
  ()

defaultSimpleWD_M2S :: SimpleWD_M2S
defaultSimpleWD_M2S = WriteData.M2S_WriteData {
    _wdata = 0,
    _wlast = False,
    _wuser = ()
  }

type SimpleWR_M2S = WriteResponse.M2S_WriteResponse
type SimpleWR_S2M = WriteResponse.S2M_WriteResponse
  ('Axi4.KeepResponse)
  ('Axi4.IdWidth 1)
  ()

-- Dit klopt sowieso niet, want nu stuurt dus een links component zowel de readdata als
-- het read address naar rechts...
type SimpleAXI dom = (SimpleRA dom, SimpleRD dom, SimpleWA dom, SimpleWD dom, SimpleWR dom)
type SimpleAXI_Data = (SimpleRA_M2S, SimpleRD_S2M, SimpleWA_M2S, SimpleWD_M2S, SimpleWR_S2M)
type SimpleAXI_Ack = (SimpleRA_S2M, SimpleRD_M2S, SimpleWA_S2M, SimpleWD_S2M, SimpleWR_M2S)



simpleBlockRAMCircuit :: HiddenClockResetEnable dom =>
  Circuit (SimpleAXI dom) ()
simpleBlockRAMCircuit = Circuit (unbundle' . simpleBlockRAM . bundle')
  where
    unbundle' a = (unbundle a, ())
    bundle' (a, ()) = bundle a

simpleBlockRAM :: HiddenClockResetEnable dom =>
  Signal dom (SimpleAXI_Data) ->
  Signal dom (SimpleAXI_Ack)
simpleBlockRAM inputs = undefined
  where
    (ra, rd, wa, wd, wr) = unbundle inputs
