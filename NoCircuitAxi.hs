{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Extraction where

import Clash.Prelude
import Protocols.Axi4.Common
import Protocols.Axi4.Lite.Axi4Lite
import Protocols.Internal
import Data.List as L


type S2M = S2M_Axi4Lite ('AddrWidth 4) 'Width32
type M2S = M2S_Axi4Lite ('AddrWidth 4) 'Width32

topEntity = exposeClockResetEnable (top @System) clockGen resetGen enableGen

top :: HiddenClockResetEnable dom =>
  Signal dom Bool -> Signal dom (S2M, M2S)
top enable = bundle (s2m, m2s)
  where
    master = mealy axiMasterMealy SendingRA
    slave = mealy axiSlaveMealy (Startup 5)

    s2m = slave m2s
    m2s = master $ bundle (s2m, enable)


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
        then Process 4
        else ReadyForAddress
      Process n -> if n <= 0
        then OutputReadData
        else Process (n - 1)
      OutputReadData -> if rdReady
        then TransactionFinishedSlave
        else OutputReadData
      TransactionFinishedSlave -> TransactionFinishedSlave

data MasterState
  = SendingRA
  | ArbitraryProcessing Int
  | SetReady
  | TransactionFinished
  deriving (Show, Generic, NFDataX)


axiMasterMealy :: MasterState -> (S2M_Axi4Lite ('AddrWidth 4) 'Width32, Bool) ->
  (MasterState, M2S_Axi4Lite ('AddrWidth 4) 'Width32)
axiMasterMealy state (S2M_Axi4Lite{..}, enable) = (state', channels)
  where
    state'' = if enable then state' else state

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
