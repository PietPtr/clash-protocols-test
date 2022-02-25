{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Axi4LiteAll where

import Clash.Prelude

import Prelude hiding ((!!), replicate, head, length, take, undefined)
import qualified Prelude as P
import Protocols
import Protocols.Internal
import Protocols.Axi4.Common
import Protocols.Axi4.Lite.Axi4Lite


master :: HiddenClockResetEnable dom =>
  Circuit () (Axi4Lite dom ('AddrWidth 4) 'Width32)
master = Circuit go
  where
    go ((), s2m) = ((), undefined)

slave :: HiddenClockResetEnable dom =>
  Circuit (Axi4Lite dom ('AddrWidth 4) 'Width32) ()
slave = Circuit go
  where
    go (m2s, ()) = (undefined, ())

inter :: HiddenClockResetEnable dom =>
  Circuit (Axi4Lite dom ('AddrWidth 4) 'Width32) (Axi4Lite dom ('AddrWidth 4) 'Width32)
inter = Circuit go
  where
    go (m2s, s2m) = (s2m, m2s)


-- data TestIdeetje (dom :: Domain) (aw :: AddrWidth) (bw :: BusWidth)

-- instance Protocol (TestIdeetje dom aw bw) where
--   type Fwd (TestIdeetje dom aw bw) = ()
--   type Bwd (TestIdeetje dom aw bw) = Bwd (Axi4Lite dom aw bw)

instance BiSimulate (Axi4Lite dom aw bw) where
  type SimulateTypeFwd (Axi4Lite dom aw bw) = [M2S_Axi4Lite aw bw]
  type SimulateTypeBwd (Axi4Lite dom aw bw) = [S2M_Axi4Lite aw bw]

  -- type ExpectProtocolFwd (Axi4Lite dom aw bw) = TestIdeetje dom aw bw

  driveFwd :: SimulationConfig
     -> [M2S_Axi4Lite ('AddrWidth 4) 'Width32]
     -> Circuit
          (ExpectProtocolFwd (Axi4Lite System ('AddrWidth 4) 'Width32))
          (Axi4Lite System ('AddrWidth 4) 'Width32)

