module DfMem where

import Clash.Prelude hiding (rom)
import Protocols
import Protocols.Df hiding (fst)
import Protocols.Internal
import Data.List as L

{-
{signal: [
  {name: 'clk', wave: 'p...........'},
  {name: 'addrIn (M->S)',  wave: 'xx2xxxxxxxxx'},
  {name: 'addrAck (S->M)', wave: '1..0.....1..'},
  {name: 'dataAck (M->S)', wave: '0.......10..'},
  {name: 'dataOut (S->M)', wave: 'xxxxxx2..xxx'}
]}
-}

sim = uncurry L.zip $ fst $ simulateCircuit (datas, acks) () (topEntity)
  where
    datas = [
        NoData,
        NoData,
        Data 4,
        NoData,

        NoData,
        NoData,
        NoData,
        NoData,

        NoData,
        NoData,
        NoData
      ]
    acks = [
        Ack False,
        Ack False,
        Ack False,
        Ack False,

        Ack False,
        Ack False,
        Ack False,
        Ack False,

        Ack True,
        Ack False,
        Ack False
      ]
    topEntity = (exposeClockResetEnable @System rom) clockGen resetGen enableGen

rom :: HiddenClockResetEnable dom => Circuit (Df dom Int, DfBwd dom (BitVector 4)) ()
rom = Circuit go
  where
    go (sigs, ()) = (unbundle $ system $ bundle sigs, ())
    system = mealy romMachine Idle

data RomState
  = Idle
  | SendDataAfter Int Int
  deriving (Generic, Show, NFDataX)

romMachine :: RomState -> (Data Int, Ack) -> (RomState, (Ack, Data (BitVector 4)))
romMachine state (addrIn, dataAck) = (state', (addrAck, dataOut))
  where
    state' = case state of
      Idle -> case addrIn of
        NoData -> Idle
        Data addr -> SendDataAfter 3 addr
      SendDataAfter 0 addr -> case dataAck of
        Ack True -> Idle
        Ack False -> SendDataAfter 0 addr
      SendDataAfter n addr -> SendDataAfter (n - 1) addr

    addrAck = case state of
      Idle -> Ack True
      SendDataAfter _ _ -> Ack False

    dataOut = case state of
      SendDataAfter 0 addr -> Data (fromIntegral $ addr * 3)
      _ -> NoData
