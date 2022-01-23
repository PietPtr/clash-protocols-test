{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module FIFO where

-- TODO: imports netter maken
import Clash.Prelude hiding (zip, undefined)
import Prelude hiding ((!!), replicate)
import qualified Prelude as P
import Protocols
import Protocols.Axi4.Common
import Protocols.Axi4.Lite.Axi4Lite
import Debug.Trace
import Data.Proxy
import Data.Coerce
import qualified Data.Maybe as Maybe

import qualified Protocols.Hedgehog as H
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Tasty
import Test.Tasty.TH (testGroupGenerator)
import Test.Tasty.Hedgehog.Extra (testProperty)

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
        addr 0,
        addr 1,
        addr 2,
        addr 3,
        noAddr,
        noAddr,
        noAddr,
        noAddr,
        noAddr,
        noAddr,
        noAddr
      ] P.++ P.repeat noAddr

    noAddr = M2S_NoWriteAddress
    addr n = M2S_WriteAddress {
        _awaddr = n,
        _awprot = (NotPrivileged, NonSecure, Data)
      }

    s2m = [
        r False,
        r False,
        r False,
        r False,
        r False,
        r False,
        r False,
        r False,
        r False,
        r False,
        r False,
        r True,
        r True,
        r True,
        r True,
        r True,
        r True,
        r True,
        r True,
        r True,
        r True,
        r False,
        r False,
        r False
      ]

    r b = S2M_WriteAddress { _awready = b }

simSignals = simulate @System top (zip (fst testSigs) (snd testSigs))
  where
    top = bundle . (toSignals axiFIFOCircuit) . unbundle



axiFIFOlhs :: HiddenClockResetEnable dom =>
  Signal dom (M2S_WriteAddress ('AddrWidth 4), Bool) ->
  Signal dom (S2M_WriteAddress, Bool, Elm)
axiFIFOlhs inp = outp
  where
    (lhsData, full) = unbundle inp
    outp = bundle (lhsReady, write, dataInFifo)
    lhsReady = (S2M_WriteAddress . not) <$> full
    (write, dataInFifo) = unbundle $ fifoWriteSigs <$> full <*> lhsData

    fifoWriteSigs full lhsData = if full
      then (False, M2S_NoWriteAddress)
      else case lhsData of
        M2S_NoWriteAddress -> (False, M2S_NoWriteAddress)
        w@(M2S_WriteAddress {}) -> (True, w)

axiFIFOrhs :: HiddenClockResetEnable dom =>
  Signal dom (S2M_WriteAddress, Bool, Elm) ->
  Signal dom (M2S_WriteAddress ('AddrWidth 4), Bool)
axiFIFOrhs inp = outp
  where
    (rhsReady, empty, dataOutFifo) = unbundle inp
    outp = bundle (rhsData, read)
    rhsData = dataSig <$> read <*> dataOutFifo
    read = readSig <$> rhsReady <*> empty

    -- Might be neater with the toBool function of a Df instance
    readSig (S2M_WriteAddress {..}) empty = _awready && not empty
    dataSig read dataOutFifo = if read
      then dataOutFifo
      else M2S_NoWriteAddress

axiFIFOCircuit :: HiddenClockResetEnable dom =>
  Circuit (Axi4LiteWA dom ('AddrWidth 4)) (Axi4LiteWA dom ('AddrWidth 4))
axiFIFOCircuit = Circuit go
  where
    go (lhsData, rhsReady) = (lhsReady, rhsData)
      where
        (lhsReady, write, dataInFifo) = unbundle $ axiFIFOlhs $ bundle (lhsData, full)
        (rhsData, read) = unbundle $ axiFIFOrhs $ bundle (rhsReady, empty, dataOutFifo)
        (full, empty, dataOutFifo) = unbundle $ fifoL $ bundle (dataInFifo, write, read)

type Elm  = M2S_WriteAddress ('AddrWidth 4)
type Pntr n = Unsigned (n + 1)

-- shorthand for testing
m2swa addr = M2S_WriteAddress {
        _awaddr = addr,
        _awprot = (NotPrivileged, NonSecure, Data)
      }

fifo :: forall n e . (KnownNat n, KnownNat (n+1), KnownNat (n+1+1)
                     ,KnownNat (n+1+2), KnownNat (2^n), Show e)
     => (Pntr n, Pntr n, Vec (2^n) e)
     -> (e, Bool, Bool)
     -> ((Pntr n,Pntr n,Vec (2^n) e),(Bool,Bool,e))
fifo (rpntr, wpntr, elms) (datain,wrt,rd) = trace (show $ (rpntr, wpntr, full, empty)) ((rpntr',wpntr',elms'),(full,empty,dataout))
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

data FIFOCommand e
  = Nop
  | Write e
  | Read
  deriving (Show, Generic, NFDataX)

data FIFOStatus
  = Full
  | Empty
  | NonEmpty
  deriving (Show, Generic, NFDataX)

strictFIFO :: forall n e . (KnownNat n, KnownNat (n+1), KnownNat (n+1+1)
                     ,KnownNat (n+1+2), KnownNat (2^n))
      => (Pntr n, Pntr n, Vec (2^n) e)
      -> (FIFOCommand e)
      -> ((Pntr n, Pntr n, Vec (2^n) e), (Maybe e, FIFOStatus))
strictFIFO (rpntr, wpntr, elms) command = ((rpntr', wpntr', elms'), (outData, status))
  where
    wpntr' | write     = wpntr + 1
           | otherwise = wpntr
    rpntr' | read      = rpntr + 1
           | otherwise = rpntr

    write = case command of
      Write _ -> True
      _ -> False

    read = case command of
      Read -> True
      _ -> False

    mask  = resize (maxBound :: Unsigned n)
    wind  = wpntr .&. mask
    rind  = rpntr .&. mask

    elms' = case command of
      Write d -> replace wind d elms
      _ -> elms

    n = fromInteger $ snatToInteger (SNat :: SNat n)

    empty = wpntr == rpntr
    full  = (testBit wpntr n) /= (testBit rpntr n) &&
            (wind == rind)

    status = case (empty, full) of
      (True, False) -> Empty
      (False, False) -> NonEmpty
      (False, True) -> Full
      (True, True) -> errorX "Invalid empty/full calculation in strict FIFO."

    outData = case command of
      Read -> Just $ elms !! rind
      _ -> Nothing

strictFIFOL :: HiddenClockResetEnable dom =>
  Signal dom (FIFOCommand Elm) -> Signal dom (Maybe Elm, FIFOStatus)
strictFIFOL = strictFIFO `mealy` (0,0,replicate d4 M2S_NoWriteAddress)


genCatMaybesInput :: H.Gen [Maybe Int]
genCatMaybesInput =
  Gen.list (Range.linear 0 100) (genMaybe (genInt 10 20))
  where
    genMaybe genA = Gen.choice [Gen.constant Nothing, Just <$> genA]
    genInt a b = Gen.integral (Range.linear a b)

genAxiFIFOInput :: H.Gen [M2S_WriteAddress ('AddrWidth 4)]
genAxiFIFOInput =
  Gen.list (Range.linear 0 100) (genM2S (genInt 0 15))
  where
    genM2S genA = Gen.choice [Gen.constant M2S_NoWriteAddress, m2swa <$> genA]
    genInt a b = Gen.integral (Range.linear a b)

-- prop_axiFIFO :: H.Property
-- prop_axiFIFO =
--   H.idWithModel
--     H.defExpectOptions
--     genAxiFIFOInput
--     id
--     (axiFIFOCircuit @System)

instance Backpressure (Axi4LiteWA dom addr) where
  boolsToBwd _ = fromList_lazy . (P.map S2M_WriteAddress)

waToMaybe :: (M2S_WriteAddress addr) -> Maybe (M2S_WriteAddress addr)
waToMaybe wa = case wa of
  M2S_NoWriteAddress -> Nothing
  w@M2S_WriteAddress {} -> Just w

instance (KnownDomain dom) => Simulate (Axi4LiteWA dom addr) where
  type SimulateType (Axi4LiteWA dom addr) = [M2S_WriteAddress addr]
  -- NoWriteAddress filtered out, not visible on typelevel..?
  type ExpectType (Axi4LiteWA dom addr) = [M2S_WriteAddress addr]
  type SimulateChannels (Axi4LiteWA dom addr) = 1

  toSimulateType Proxy = id
  fromSimulateType Proxy = Maybe.mapMaybe waToMaybe

  driveC = undefined
  sampleC = undefined -- sample
  stallC = undefined
  -- stallC conf (C.head -> (stallAck, stalls)) = stall conf stallAck stalls

