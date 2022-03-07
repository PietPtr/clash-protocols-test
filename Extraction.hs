{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Extraction where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Common
import Protocols.Axi4.Lite.Axi4Lite
import Protocols.Internal
import Data.List as L

data SignalL dom a = SignalL (Signal dom a)
data SignalR dom a = SignalR (Signal dom a)

-- werkt niet door injectivity
-- instance Protocol (SignalR dom a) where
--   type Fwd (SignalR dom a) = SignalR dom ()
--   type Bwd (SignalR dom a) = SignalR dom a

instance Protocol (SignalL dom a) where
  type Fwd (SignalL dom a) = SignalL dom a
  type Bwd (SignalL dom a) = SignalL dom ()


type InputType = BitVector 4

axiSlave :: HiddenClockResetEnable dom =>
  Circuit (Axi4Lite dom ('AddrWidth 4) ('Width32), CSignal dom InputType) ()
axiSlave = Circuit go
  where
    go ((m2s, otherInp), ()) = ((machine $ bundle (m2s, toSig otherInp), undefined), ())
    machine = mealy axiSlaveMealy (Startup 101)

    toSig (CSignal sig) = sig

data SlaveState
  = Startup Int
  | ReadyForAddress
  | Process Int
  | OutputReadData
  | TransactionFinishedSlave
  deriving (Show, Generic, NFDataX)

axiSlaveMealy :: SlaveState -> (M2S_Axi4Lite ('AddrWidth 4) 'Width32, InputType) ->
  (SlaveState, S2M_Axi4Lite ('AddrWidth 4) 'Width32)
axiSlaveMealy state (M2S_Axi4Lite{..}, _) = (state', channels)
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
