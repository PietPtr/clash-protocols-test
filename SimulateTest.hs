module SimulateTest where

import Clash.Prelude
import Protocols
import Protocols.Df hiding (zip, map)
import Protocols.Internal
import Data.List as L

circuit' :: HiddenClockResetEnable dom =>
  Circuit (Df dom Int) (Df dom Int)
circuit' = registerFwd

sim :: [(Ack, Data Int)]
sim = uncurry L.zip $ simulateC' def
  [Protocols.Df.Data 1, Protocols.Df.Data 2, NoData, NoData, NoData, NoData]
  [Ack False, Ack False, Ack False, Ack False, Ack False, Ack True, Ack False, Ack True]
  (withClockResetEnable clockGen resetGen enableGen $ circuit' @System)
