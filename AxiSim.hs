{-# LANGUAGE RecordWildCards #-}
module AxiSim where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Common
import Protocols.Axi4.Lite.Axi4Lite
import Protocols.Internal
import Data.List as L

idleS2MChannels :: S2M_Axi4Lite ('AddrWidth 4) ('Width32)
idleS2MChannels = S2M_Axi4Lite {
    s2m_wa = S2M_WriteAddress False,
    s2m_wd = S2M_WriteData False,
    s2m_wr = S2M_NoWriteResponse,
    s2m_ra = S2M_ReadAddress False,
    s2m_rd = S2M_NoReadData
  }

idle = L.repeat idleS2MChannels


sim = simulateManager def testCase master
  where
    master = exposeClockResetEnable (axiMaster @System) clockGen resetGen enableGen

sim' = simulateSubordinate def testCaseSlave slave
  where
    slave = exposeClockResetEnable (axiSlave @System) clockGen resetGen enableGen

testCaseSlave = [
    build False noaddr,
    build False (buildra 5),
    build False (buildra 5),
    build False (buildra 5),
    build False noaddr,
    build False noaddr,
    build False noaddr,
    build False noaddr,
    build True noaddr,
    build True noaddr,
    build True noaddr,
    build True noaddr,
    build True noaddr,
    build True noaddr,
    build True noaddr
  ]
  where
    build rdready addr = M2S_Axi4Lite {
      m2s_wa = M2S_NoWriteAddress,
      m2s_wd = M2S_NoWriteData,
      m2s_wr = M2S_WriteResponse False,
      m2s_ra = addr,
      m2s_rd = M2S_ReadData rdready
    }

    noaddr = M2S_NoReadAddress

    buildra value = M2S_ReadAddress {
      _araddr = value,
      _arprot = (NotPrivileged, NonSecure, Data)
    }

axiSlave :: HiddenClockResetEnable dom =>
  Circuit (Axi4Lite dom ('AddrWidth 4) ('Width32)) ()
axiSlave = Circuit go
  where
    go (m2s, ()) = (machine m2s, ())
    machine = mealy axiSlaveMealy (Startup 100)

data SlaveState
  = Startup Int
  | ReadyForAddress
  | Process Int
  | OutputReadData
  | TransactionFinishedSlave
  deriving (Show, Generic, NFDataX)

axiSlaveMealy :: SlaveState -> (M2S_Axi4Lite ('AddrWidth 4) 'Width32) ->
  (SlaveState, S2M_Axi4Lite ('AddrWidth 4) 'Width32)
axiSlaveMealy state M2S_Axi4Lite{..} = (state', channels)
  where
    channels = S2M_Axi4Lite {
      s2m_wa = S2M_WriteAddress False,
      s2m_wd = S2M_WriteData False,
      s2m_wr = S2M_NoWriteResponse,
      s2m_ra = S2M_ReadAddress s2m_ra,
      s2m_rd = s2m_rd
    }

    s2m_ra = case state of
      ReadyForAddress -> True
      _ -> False
    s2m_rd = case state of
      OutputReadData -> S2M_ReadData {
          _rdata = 0:>0:>0:>7:>Nil,
          _rresp = RLOkay
        }
      _ -> S2M_NoReadData

    raHasData = case m2s_ra of
      M2S_NoReadAddress -> False
      _ -> True

    rdReady = _rready m2s_rd

    state' = case state of
      Startup n -> if n <= 0
        then ReadyForAddress
        else Startup (n - 1)
      ReadyForAddress -> if raHasData
        then ReadyForAddress
        else Process 4
      Process n -> if n <= 0
        then OutputReadData
        else Process (n - 1)
      OutputReadData -> if rdReady
        then TransactionFinishedSlave
        else OutputReadData
      TransactionFinishedSlave -> TransactionFinishedSlave

axiMaster :: HiddenClockResetEnable dom =>
  Circuit () (Axi4Lite dom ('AddrWidth 4) ('Width32))
axiMaster = Circuit go
  where
    go ((), s2m) = ((), machine s2m)
    machine = mealy axiMasterMealy (SendingRA)

data MasterState
  = SendingRA
  | ArbitraryProcessing Int
  | SetReady
  | TransactionFinished
  deriving (Show, Generic, NFDataX)


axiMasterMealy :: MasterState -> (S2M_Axi4Lite ('AddrWidth 4) 'Width32) ->
  (MasterState, M2S_Axi4Lite ('AddrWidth 4) 'Width32)
axiMasterMealy state S2M_Axi4Lite{..} = (state', channels)
  where
    channels = M2S_Axi4Lite {
      m2s_wa = M2S_NoWriteAddress,
      m2s_wd = M2S_NoWriteData,
      m2s_wr = M2S_WriteResponse False,
      m2s_ra = m2s_ra,
      m2s_rd = M2S_ReadData m2s_rd
    }

    m2s_ra = case state of
      SendingRA -> M2S_ReadAddress {
          _araddr = 5,
          _arprot = (NotPrivileged, NonSecure, Data)
        }
      _ -> M2S_NoReadAddress

    m2s_rd = case state of
      SetReady -> True
      _ -> False

    gotData = case s2m_rd of
      S2M_ReadData {} -> True
      _ -> False

    isRAReady = _arready s2m_ra

    state' = case state of
      SendingRA -> if isRAReady
        then ArbitraryProcessing 4
        else SendingRA
      ArbitraryProcessing n -> if n == 0
        then SetReady
        else ArbitraryProcessing (n - 1)
      SetReady -> if gotData
        then TransactionFinished
        else SetReady
      TransactionFinished -> TransactionFinished

testCase :: [S2M_Axi4Lite ('AddrWidth 4) 'Width32]
testCase = [
    build False nodata,
    build False nodata,
    build True nodata,
    build False nodata,
    build False nodata,
    build False nodata,
    build False (buildrd 5),
    build False (buildrd 5),
    build False (buildrd 5),
    build False (buildrd 5),
    build False (buildrd 5)
  ]
  where
    build arready d = S2M_Axi4Lite {
        s2m_wa = S2M_WriteAddress False,
        s2m_wd = S2M_WriteData False,
        s2m_wr = S2M_NoWriteResponse,
        s2m_ra = S2M_ReadAddress arready,
        s2m_rd = d
    }

    buildrd value = S2M_ReadData {
        _rdata = 0:>0:>0:>value:>Nil,
        _rresp = RLOkay
      }

    nodata = S2M_NoReadData
