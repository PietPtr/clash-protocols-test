module FIFO where


import Clash.Prelude hiding (zip, undefined)
import Prelude hiding ((!!), replicate)
import qualified Prelude as P
import Protocols
import Protocols.Axi4.Common
import Protocols.Axi4.Lite.Axi4Lite
import Debug.Trace

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (M2S_WriteAddress ('AddrWidth 4), S2M_WriteAddress) ->
  Signal System (S2M_WriteAddress, M2S_WriteAddress ('AddrWidth 4))
topEntity = (exposeClockResetEnable $ bundle . toSignals axiFIFOCircuit . unbundle)


testSigs :: ([M2S_WriteAddress ('AddrWidth 4)], [S2M_WriteAddress])
testSigs = (m2s, s2m)
  where
    m2s = [
        noAddr,
        noAddr,
        addr 0,
        noAddr,
        noAddr,
        addr 1,
        addr 2,
        addr 3
      ]
    
    noAddr = M2S_NoWriteAddress
    addr n = M2S_WriteAddress {
        _awaddr = n,
        _awprot = (NotPrivileged, NonSecure, Data)
      }

    s2m = [
        r False,
        r False,
        r False,
        r False 
      ]
    
    r b = S2M_WriteAddress { _awready = b }

simSignals = simulate @System top (zip (fst testSigs) (snd testSigs))
  where
    top = bundle . axiFIFOWrapper . unbundle


axiFIFOCircuit :: HiddenClockResetEnable dom
  => Circuit (Axi4LiteWA dom ('AddrWidth 4)) (Axi4LiteWA dom ('AddrWidth 4))
axiFIFOCircuit = fromSignals axiFIFOWrapper

axiFIFOWrapper :: HiddenClockResetEnable dom
  => (Signal dom (M2S_WriteAddress ('AddrWidth 4)), Signal dom S2M_WriteAddress)
  -> (Signal dom S2M_WriteAddress, Signal dom (M2S_WriteAddress ('AddrWidth 4)))
axiFIFOWrapper wrapperIn = unbundle wrapperOut
  where
    (wrapperOut, fifoIn) = unbundle $ machine $ bundle (bundle wrapperIn, fifoOut)
    fifoOut = fifoL fifoIn

    machine = mealy axiFIFOWrapperMealy ()

type AxiFIFOWrapperState = ()
type AxiFIFOWrapperInput = (M2S_WriteAddress ('AddrWidth 4), S2M_WriteAddress)
type AxiFIFOWrapperOutput = (S2M_WriteAddress, M2S_WriteAddress ('AddrWidth 4))

axiFIFOWrapperMealy :: AxiFIFOWrapperState -> (AxiFIFOWrapperInput, (Bool, Bool, Elm)) 
  -> (AxiFIFOWrapperState, (AxiFIFOWrapperOutput, (Elm, Bool, Bool)))
axiFIFOWrapperMealy state ((lhsData, rhsReady), (full, empty, dataOut)) = 
  (state', ((lhsReady, rhsData), (dataIn, write, read)))
  where
    state' = state

    lhsReady = S2M_WriteAddress {
      _awready = writeReady
    }
    writeReady = not full -- TODO: check reset behaviour

    rhsData = if empty
      then M2S_NoWriteAddress
      else dataOut


    dataIn = lhsData
    write = lhsValid && writeReady -- = handshake, data is valid and we are ready to write
    
    lhsValid = case lhsData of
      M2S_NoWriteAddress -> False
      M2S_WriteAddress {} -> True

    read = not empty && rhsReadyBool -- kort door de bocht?

    rhsReadyBool = _awready rhsReady


type Elm  = M2S_WriteAddress ('AddrWidth 4)
type Pntr n = Unsigned (n + 1)


fifo :: forall n e . (KnownNat n, KnownNat (n+1), KnownNat (n+1+1)
                     ,KnownNat (n+1+2), KnownNat (2^n))
     => (Pntr n, Pntr n, Vec (2^n) e)
     -> (e, Bool, Bool)
     -> ((Pntr n,Pntr n,Vec (2^n) e),(Bool,Bool,e))
fifo (rpntr, wpntr, elms) (datain,wrt,rd) = ((rpntr',wpntr',elms'),(full,empty,dataout))
  where
    wpntr' | wrt       = wpntr + 1
           | otherwise = wpntr
    rpntr' | rd        = rpntr + 1
           | otherwise = rpntr

    mask  = resize (maxBound :: Unsigned n)
    wind  = wpntr .&. mask
    rind  = rpntr .&. mask

    elms' | wrt       = replace wind datain elms
          | otherwise = elms

    n = fromInteger $ snatToInteger (SNat :: SNat n)

    empty = wpntr == rpntr
    full  = (testBit wpntr n) /= (testBit rpntr n) &&
            (wind == rind)

    dataout = elms !! rind

fifoL :: HiddenClockResetEnable dom =>
  Signal dom (Elm,Bool,Bool) -> Signal dom (Bool,Bool,Elm)
fifoL = fifo `mealy` (0,0,replicate d4 M2S_NoWriteAddress)
