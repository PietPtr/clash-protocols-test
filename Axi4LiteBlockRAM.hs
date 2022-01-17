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

module Axi4LiteBlockRAM where

import Clash.Prelude hiding (map, zip, undefined, take)
import qualified Clash.Prelude as C

import Protocols
import Protocols.Axi4.Common
import Protocols.Axi4.Lite.Axi4Lite


type SimpleLite dom = Axi4Lite dom ('AddrWidth 4) 'Width32
type SimpleLite_M2S = M2S_Axi4Lite ('AddrWidth 4) 'Width32
type SimpleLite_S2M = S2M_Axi4Lite ('AddrWidth 4) 'Width32

blockRAMLiteCircuit :: HiddenClockResetEnable dom =>
  Circuit (SimpleLite dom) ()
blockRAMLiteCircuit = Circuit go
  where
    go (s, ()) = (blockRAMLite s, ())

blockRAMLite :: HiddenClockResetEnable dom =>
  Signal dom SimpleLite_M2S -> Signal dom SimpleLite_S2M
blockRAMLite = mealy blockram (C.repeat 0)

type BlockRAMState = Vec 16 (BitVector 8)

blockram :: BlockRAMState -> SimpleLite_M2S -> (BlockRAMState, SimpleLite_S2M)
blockram state input = (state', output)
  where
    M2S_Axi4Lite {..} = input
    state' = state
    output = S2M_Axi4Lite {
      _write_address_ack = S2M_WriteAddress {
        _awready = True
      },
      _write_data_ack = S2M_WriteData {
        _wready = True
      },
      _write_response = S2M_NoWriteResponse,

    }
