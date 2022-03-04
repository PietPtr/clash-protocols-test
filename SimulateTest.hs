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

simMan = simulateManager def
  (Ack <$> [True, True, False, False, False, False, True])
  (withClockResetEnable clockGen resetGen enableGen $ circ @System)
  where
    circ :: HiddenClockResetEnable dom =>
      Circuit () (Df dom Int)
    circ = (drive def [Data 1, Data 2, Data 3]) |> registerFwd

simSub = simulateSubordinate def
  ([NoData, Data 10, Data 5, Data 4, Data 3])
  (withClockResetEnable clockGen resetGen enableGen $ circ @System)
  where
    circ :: HiddenClockResetEnable dom =>
      Circuit (Df dom Int) ()
    circ = Circuit go
      where
        go (dataSig, ()) = (ackSig, ())
          where
            ackSig = (\r -> Ack $ r <= 0) <$> regSig

            regSig = register 0 setSig

            setSig = setter <$> dataSig <*> regSig
            setter d current = if current == 0
              then case d of
                Data n -> n
                NoData -> current
              else current - 1

