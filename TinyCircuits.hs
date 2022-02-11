module TinyCircuits where

import Clash.Prelude

import Protocols
import Protocols.Df hiding (simulate, sample)

oneReg :: HiddenClockResetEnable dom =>
  Signal dom (Data Int, Ack) -> Signal dom (Ack, Data Int)
oneReg = bundle . toSignals reg . unbundle
  where
    reg = registerFwd @_ @Int

simOneReg = simulate @System oneReg [(NoData, Ack False), (NoData, Ack False)]


fanoutTest' :: HiddenClockResetEnable dom =>
  -- Signal dom (Data Int, Ack) -> Signal dom (Ack, Vec 2 (Data Int))
  Circuit (Df dom Int) (Vec 2 (Df dom Int))
fanoutTest' = fanout

fanoutTest :: HiddenClockResetEnable dom =>
  (Signal dom (Data Int), Vec 2 (Signal dom Ack)) ->
  (Signal dom Ack)
fanoutTest (inData, ackVec) = lhsAck
  where
    (lhsAck, _) = (toSignals fanout) (inData, ackVec)

simFanout = sample $ sys input
  where
    input = (fromList [NoData, Data 1, NoData], acks :> acks :> Nil)

    acks = fromList [Ack True, Ack True, Ack True]

    sys = exposeClockResetEnable (fanoutTest @System) clockGen resetGen enableGen
