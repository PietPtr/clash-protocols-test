{-# LANGUAGE RecordWildCards #-}

module Protocols.Axi4.Lite.Example where

import Clash.Prelude hiding (zip, undefined)
import Prelude hiding ((!!))
import qualified Prelude as P
import Protocols
import Protocols.Axi4.Common
import Protocols.Axi4.Lite.Axi4Lite


type BasicAddrWidth = BitVector (Width ('AddrWidth 4))

data BasicAxiMaster
  = BM_Read BasicAddrWidth
  | BM_Write BasicAddrWidth (WriteBusWidthType 'Width32)
  | BM_NoData
  deriving (Show, Generic, NFDataX)

data BasicAxiSlave
  = BS_Read (ReadBusWidthType 'Width32)
  | BS_Idle
  | BS_Busy
  deriving (Show, Generic, NFDataX)

data BasicAxi (dom :: Domain)

instance Protocol (BasicAxi dom) where
  type Fwd (BasicAxi dom) = Signal dom BasicAxiMaster
  type Bwd (BasicAxi dom) = Signal dom BasicAxiSlave

type AxiToBasic dom = Circuit
  (Axi4Lite dom ('AddrWidth 4) 'Width32)
  (BasicAxi dom)

masterTestWriteSigs :: [(M2S_WriteAddress ('AddrWidth 4), M2S_WriteData 'Width32, M2S_WriteResponse)]
masterTestWriteSigs = P.zip3 wa wd wr
  where
    wa =
      [ M2S_NoWriteAddress
      , M2S_NoWriteAddress
      , m2s_wa 0
      , M2S_NoWriteAddress
      , M2S_NoWriteAddress
      , M2S_NoWriteAddress
      , M2S_NoWriteAddress
      , M2S_NoWriteAddress ]
    wd =
      [ M2S_NoWriteData
      , M2S_NoWriteData
      , m2s_wd 255
      , M2S_NoWriteData
      , M2S_NoWriteData
      , M2S_NoWriteData ]
    wr = P.repeat $ M2S_WriteResponse { _bready = True }
    m2s_wa addr = M2S_WriteAddress { _awaddr = addr, _awprot = (NotPrivileged, NonSecure, Data)}
    m2s_wd d = M2S_WriteData { _wdata = Nothing:>Nothing:>Nothing:>(Just d):>Nil }

masterTestSigs :: [(M2S_ReadAddress ('AddrWidth 4), M2S_ReadData 'Width32)]
masterTestSigs = zip ra rd
  where
    ra =
      [ M2S_NoReadAddress
      , m2s_ra 3
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , m2s_ra 4
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress
      , M2S_NoReadAddress]
    rd =
      [ nack, nack, nack, nack, nack, nack, nack, nack, ack, nack, nack, nack, nack, ack ]
      P.++ P.repeat nack

    m2s_ra addr = M2S_ReadAddress { _araddr = addr, _arprot = (NotPrivileged, NonSecure, Data)}
    ack = M2S_ReadData { _rready = True }
    nack = M2S_ReadData { _rready = False }

simRead :: [(M2S_ReadAddress ('AddrWidth 4), M2S_ReadData 'Width32)]
  -> [(S2M_ReadAddress, S2M_ReadData 'Width32)]
simRead = simulate @System (bundle . top' . unbundle)
  where
    top :: HiddenClockResetEnable dom =>
      ((Signal dom (M2S_ReadAddress ('AddrWidth 4)),
       Signal dom (M2S_ReadData 'Width32)),
      ())
     -> ((Signal dom S2M_ReadAddress,
          Signal dom (S2M_ReadData 'Width32)),
         ())
    top = toSignals $ convertRead |> basicAxiMem
    top' sigs = case top (sigs, ()) of
      (sigs', ()) -> sigs'


simWrite = simulate @System (bundle . top' . unbundle)
  where
    top :: HiddenClockResetEnable dom =>
     ((Signal dom (M2S_WriteAddress ('AddrWidth 4)),
       Signal dom (M2S_WriteData 'Width32), Signal dom M2S_WriteResponse),
      ())
     -> ((Signal dom S2M_WriteAddress, Signal dom S2M_WriteData,
          Signal dom S2M_WriteResponse),
         ())
    top = toSignals $ convertWrite |> basicAxiMem
    top' sigs = case top (sigs, ()) of
      (sigs', ()) -> sigs'

-- converter :: AxiToBasic dom
-- converter = Circuit go

convertWrite :: HiddenClockResetEnable dom =>
  Circuit (Axi4LiteWrite dom ('AddrWidth 4) 'Width32) (BasicAxi dom)
convertWrite = Circuit go
  where
    go ((wa_data, wd_data, wr_ack), basicSlave) = ((wa_ack, wd_ack, wr_data), basicMaster)
      where
        (wa_ack, wd_ack, wr_data) = unbundle writeChannels
        (writeChannels, basicMaster) = unbundle $ machine
          (bundle (bundle (wa_data, wd_data, wr_ack), basicSlave))

        machine = mealy convertWriteMealy CW_Idle

convertWAD :: M2S_WriteAddress ('AddrWidth 4) -> M2S_WriteData 'Width32 -> BasicAxiMaster
convertWAD M2S_NoWriteAddress _ = BM_NoData
convertWAD _ M2S_NoWriteData = BM_NoData
convertWAD (M2S_WriteAddress {..}) (M2S_WriteData {..}) = BM_Write _awaddr _wdata


type ConvertWriteInput = ((M2S_WriteAddress ('AddrWidth 4), M2S_WriteData 'Width32, M2S_WriteResponse), BasicAxiSlave)
type ConvertWriteOutput = ((S2M_WriteAddress, S2M_WriteData, S2M_WriteResponse), BasicAxiMaster)

data ConvertWriteState
  = CW_Idle
  | CW_KeepWA (BitVector (Width ('AddrWidth 4)))
  | CW_SendWrites
  deriving (Show, Generic, NFDataX)

convertWriteMealy :: ConvertWriteState -> ConvertWriteInput -> (ConvertWriteState, ConvertWriteOutput)
convertWriteMealy state ((wa_data, wd_data, wr_ack), basicSlave) = (state', ((wa_ack, wd_ack, wr_data), basicMaster))
  where
    state' = case state of
      CW_Idle -> case wa_data of
        M2S_WriteAddress {..} -> CW_KeepWA _awaddr
        _ -> CW_Idle
      CW_KeepWA addr -> case wd_data of
        M2S_WriteData {} -> CW_SendWrites
        _ -> CW_KeepWA addr
      CW_SendWrites -> CW_Idle

    wa_ack = case basicSlave of
      BS_Idle -> S2M_WriteAddress { _awready = True }
      _ -> S2M_WriteAddress { _awready = False }
    wd_ack = case basicSlave of
      BS_Idle -> S2M_WriteData { _wready = True }
      _ -> S2M_WriteData { _wready = False }
    wr_data = case state of
      -- must be kept on RLOkay until ack from master..
      CW_SendWrites -> S2M_WriteResponse { _bresp = RLOkay }
      _ -> S2M_NoWriteResponse

    basicMaster = case (state, wd_data) of
      (CW_KeepWA addr, M2S_WriteData {..}) -> BM_Write addr _wdata
      _ -> BM_NoData



convertRead :: HiddenClockResetEnable dom =>
  Circuit (Axi4LiteRead dom ('AddrWidth 4) 'Width32) (BasicAxi dom)
convertRead = Circuit go
  where
    go ((ra_data, rd_ack), basicSlave) = ((ra_ack, rd_data), basicMaster)
      where
        (ra_ack, rd_data) = unbundle ra_rd
        (ra_rd, basicMaster) = unbundle $ (machine)
          (bundle (bundle (ra_data, rd_ack), basicSlave))

        machine = mealy convertReadMealy CR_Idle

convertRA :: M2S_ReadAddress ('AddrWidth 4) -> BasicAxiMaster
convertRA M2S_NoReadAddress = BM_NoData
convertRA M2S_ReadAddress {..} = BM_Read _araddr

convertRD :: BasicAxiSlave -> S2M_ReadData 'Width32
convertRD basicSlave = case basicSlave of
  BS_Read d -> S2M_ReadData {
    _rdata = d,
    _rresp = RLOkay
  }
  _ -> S2M_NoReadData

type ConvertReadInput = ((M2S_ReadAddress ('AddrWidth 4), M2S_ReadData 'Width32), BasicAxiSlave)
type ConvertReadOutput = ((S2M_ReadAddress, S2M_ReadData 'Width32), BasicAxiMaster)

data ConvertReadState = CR_Idle | CR_WaitForReady BasicAxiSlave
  deriving (Show, Generic, NFDataX)

convertReadMealy :: ConvertReadState -> ConvertReadInput -> (ConvertReadState, ConvertReadOutput)
convertReadMealy (keepData) ((ra_data, rd_ack), basicSlave) = (keepData', ((ra_ack, rd_data), basicMaster))
  where
    rd_data = case keepData of
      CR_WaitForReady d -> convertRD d
      _ -> convertRD basicSlave
    ra_ack = s2m_ra basicSlave
    basicMaster = convertRA ra_data
    keepData' = case keepData of
      CR_Idle -> case basicSlave of
        BS_Read _ -> CR_WaitForReady basicSlave
        _ -> CR_Idle
      CR_WaitForReady _ -> if masterReady
        then CR_Idle
        else keepData

    masterReady = _rready rd_ack

    s2m_ra slaveCmd = case (slaveCmd, keepData) of
      (BS_Idle, CR_Idle) -> S2M_ReadAddress { _arready = True }
      _ -> S2M_ReadAddress { _arready = False }

basicAxiMem :: HiddenClockResetEnable dom =>
  Circuit (BasicAxi dom) ()
basicAxiMem = Circuit go
  where
    go (master, ()) = (memory master, ())

memory :: HiddenClockResetEnable dom =>
  Signal dom BasicAxiMaster -> Signal dom BasicAxiSlave
memory = mealy memoryMealy emptyMemoryState

data MemState = MemState {
    counter :: Unsigned 2,
    values :: Vec 16 (BitVector 8),
    last_command :: Maybe BasicAxiMaster
  } deriving (Show, NFDataX, Generic)

emptyMemoryState :: MemState
emptyMemoryState = MemState {
    counter = 0,
    values = 0:>1:>2:>3:>4:>5:>6:>7:>8:>9:>10:>11:>12:>13:>14:>15:>Nil,
    last_command = Nothing
  }

memoryMealy :: MemState -> BasicAxiMaster -> (MemState, BasicAxiSlave)
memoryMealy state command = (state', result)
  where
    MemState {..} = state
    state' = state {
      counter = counter + 1,
      last_command = case last_command of
        Nothing -> case command of
          BM_NoData -> Nothing
          _ -> Just command
        Just _ -> if counter == 0
          then case command of
            BM_NoData -> Nothing
            _ -> Just command
          else last_command,
      values = case (last_command, counter) of
        (Just (BM_Write addr d), 0) -> replace addr (setValueStb (values !! addr) d) values
        _ -> values
    }

    result = case (last_command, counter) of
      (Just cmd, 0) -> execute cmd
      (Just _, _) -> BS_Busy
      (Nothing, _) -> BS_Idle

    setValueStb :: BitVector 8 -> Vec 4 (Maybe (BitVector 8)) -> BitVector 8
    setValueStb prev vec = case vec !! 3 of
      Just b -> b
      Nothing -> prev

    execute cmd = case cmd of
      BM_Read addr -> BS_Read (0:>0:>0:>(values !! addr):>Nil)
      _ -> BS_Idle
