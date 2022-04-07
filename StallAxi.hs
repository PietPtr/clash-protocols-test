{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module StallAxi where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Common
import Protocols.Axi4.Lite.Axi4Lite
import Protocols.Internal
import Data.List as L

axiID :: Circuit
  (Axi4Lite System ('AddrWidth 4) 'Width32)
  (Axi4Lite System ('AddrWidth 4) 'Width32)
axiID = idC

sim = simulateCircuit fwd bwd stalled
  where
    fwd = testCaseSlave
    bwd = elm : elm' : elm' : elm' : L.repeat elm

    elm = S2M_Axi4Lite {
        s2m_wa = S2M_WriteAddress True,
        s2m_wd = S2M_WriteData True,
        s2m_wr = S2M_NoWriteResponse,
        s2m_ra = S2M_ReadAddress True,
        s2m_rd = S2M_NoReadData
      }

    elm' :: S2M_Axi4Lite ('AddrWidth 4) 'Width32
    elm' = S2M_Axi4Lite {
      s2m_wa = S2M_WriteAddress True,
      s2m_wd = S2M_WriteData True,
      s2m_wr = S2M_NoWriteResponse,
      s2m_ra = S2M_ReadAddress True,
      s2m_rd = S2M_ReadData (0:>1:>0:>1:>Nil) RLOkay
    }



stalled = exposeClockResetEnable (stallTest |> axiID) clockGen resetGen enableGen

stallTest :: HiddenClockResetEnable dom =>
  Circuit (Axi4Lite dom ('AddrWidth 4) ('Width32)) (Axi4Lite dom ('AddrWidth 4) ('Width32))
stallTest = stallC conf (was :> wds :> wrs :> ras :> rds :> Nil)
  where
    was = (StallWithAck, [])
    wds = (StallWithAck, [])
    wrs = (StallWithAck, [])
    ras = (StallWithAck, [4,3,4])
    rds = (StallWithAck, [2,4,5])

    conf = def { resetCycles = 0 }

testCaseSlave :: [M2S_Axi4Lite ('AddrWidth 4) 'Width32]
testCaseSlave = [
    build False noaddr,
    build False (buildra 5),
    build False (buildra 5),
    build False (buildra 5),
    build False (buildra 5),
    build False (buildra 5),
    build False noaddr,
    build False noaddr,
    build True noaddr,
    build True noaddr,
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
