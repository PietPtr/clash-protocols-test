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

module FIFO where

-- TODO: imports netter maken
import Clash.Prelude hiding (zip, undefined)
import           Clash.Signal.Internal (Signal(..))
import qualified Clash.Explicit.Prelude as CE
import Prelude hiding ((!!), replicate, head, length, take)
import qualified Prelude as P
import Protocols
import Protocols.Axi4.Common
import Protocols.Axi4.Lite.Axi4Lite
import Debug.Trace
import Data.Proxy
import Data.Coerce
import           Data.Bool (bool)
import qualified Data.Maybe as Maybe
import qualified Data.Bifunctor as B
import GHC.Stack (withFrozenCallStack, HasCallStack)

import qualified Protocols.Hedgehog as H
import qualified Hedgehog as H hiding (Test)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List ((\\))
import qualified Hedgehog.Internal.Show as H
import qualified Hedgehog.Internal.Property as H hiding (Test)

import Text.Show.Pretty (ppShow)

import Test.Tasty
import Test.Tasty.TH (testGroupGenerator)
import Test.Tasty.Hedgehog.Extra (testProperty)

{-# ANN topEntity
  (Synthesize
    { t_name   = "axi_fifo"
    , t_inputs = [PortName "clk", PortName "rst", PortName "en", PortName "in_sigs"]
    , t_output = PortName "out_sigs"
    }) #-}
topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (M2S_WriteAddress ('AddrWidth 4), S2M_WriteAddress) ->
  Signal System (S2M_WriteAddress, M2S_WriteAddress ('AddrWidth 4))
topEntity = (exposeClockResetEnable $ bundle . toSignals axiFIFOCircuit . unbundle)


noAddr = M2S_NoWriteAddress

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

-- simSignals = simulate @System top (zip (fst testSigs) (snd testSigs))
--   where
--     top = bundle . (toSignals axiFIFOCircuit) . unbundle

simCircuit :: [M2S_WriteAddress ('AddrWidth 4)]
simCircuit = P.take 130 $ simulateC (exposedAxiFIFO) def $
  [noAddr, m2swa 1, noAddr] <> P.repeat noAddr


exposedAxiFIFO :: Circuit
       (Axi4LiteWA System ('AddrWidth 4))
       (Axi4LiteWA System ('AddrWidth 4))
exposedAxiFIFO =
  -- undefined
  exposeClockResetEnable (axiFIFOCircuit) clockGen resetGen enableGen

axiFIFOlhs :: HiddenClockResetEnable dom =>
  Signal dom (M2S_WriteAddress ('AddrWidth 4), Bool) ->
  Signal dom (S2M_WriteAddress, Bool, Elm)
axiFIFOlhs inp = outp
  where
    ~(lhsData, full) = unbundle inp
    outp = bundle (lhsReady, write, dataInFifo)
    lhsReady = (S2M_WriteAddress . not) <$> full
    ~(write, dataInFifo) = unbundle $ fifoWriteSigs <$> full <*> lhsData

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
    ~(rhsReady, empty, dataOutFifo) = unbundle inp
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
    go ~(lhsData, rhsReady) = (lhsReady, rhsData)
      where
        ~(lhsReady, write, dataInFifo) = unbundle $ axiFIFOlhs $ bundle (lhsData, full)
        ~(rhsData, read) = unbundle $ axiFIFOrhs $ bundle (rhsReady, empty, dataOutFifo)
        ~(full, empty, dataOutFifo) = unbundle $ fifoL $ bundle (dataInFifo, write, read)

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
fifo ~(rpntr, wpntr, elms) ~(datain,wrt,rd) = {- trace (show (elms))-} ((rpntr',wpntr',elms'),(full,empty,dataout))
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
    genM2S genA = Gen.choice [{-Gen.constant M2S_NoWriteAddress,-} m2swa <$> genA]
    genInt a b = Gen.integral (Range.linear a b)




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

  driveC = driveWA
  sampleC = sampleWA -- sample
  stallC conf (head -> (stallAck, stalls)) = stallWA conf stallAck stalls

driveWA ::
  SimulationConfig ->
  [M2S_WriteAddress addr] ->
  Circuit () (Axi4LiteWA dom addr)
driveWA SimulationConfig{resetCycles} s0 = Circuit $
    ((),)
  . fromList_lazy
  . go s0 resetCycles
  . CE.sample_lazy
  . P.snd
  where
    go _ resetN ~(ack:acks) | resetN > 0 =
      M2S_NoWriteAddress : (ack `seqX` go s0 (resetN - 1) acks)
    go [] _ ~(ack:acks) =
      M2S_NoWriteAddress : (ack `seqX` go [] 0 acks)
    go (dat:is) _ ~(ack:acks) = case dat of
      M2S_NoWriteAddress -> M2S_NoWriteAddress : (ack `seqX` go is 0 acks)
      M2S_WriteAddress {} -> dat : go (if _awready ack then is else dat:is) 0 acks

sampleWA ::
  SimulationConfig ->
  Circuit () (Axi4LiteWA dom addr) ->
  [M2S_WriteAddress addr]
sampleWA SimulationConfig{..} c =
    P.take timeoutAfter
  $ CE.sample_lazy
  $ ignoreWhileInReset
  $ P.snd
  $ toSignals c ((), S2M_WriteAddress <$> rst_n)
  where
    ignoreWhileInReset s =
      (uncurry (bool (M2S_NoWriteAddress))) <$>
      bundle (s, rst_n)

    rst_n = fromList $ P.replicate resetCycles False <> P.repeat True

stallWA ::
  SimulationConfig ->
  StallAck ->
  [Int] ->
  Circuit (Axi4LiteWA dom addr) (Axi4LiteWA dom addr)
stallWA SimulationConfig{..} stallAck stalls = Circuit $
  uncurry (go stallAcks stalls resetCycles)
  where
    go :: [StallAck]
      -> [Int]
      -> Int
      -> Signal dom (M2S_WriteAddress addr)
      -> Signal dom S2M_WriteAddress
      -> (Signal dom S2M_WriteAddress,
          Signal dom (M2S_WriteAddress addr))
    go [] ss rs fwd bwd = go stallAcks ss rs fwd bwd

    go (_:sas) _ resetN (f :- fwd) ~(b :- bwd) | resetN > 0 =
      B.bimap (b :-) (f :-) (go sas stalls (resetN - 1) fwd bwd)

    go (sa:sas) [] _ (f :- fwd) ~(b :- bwd) =
      B.bimap (toStallAck f b sa :-) (f :-) (go sas [] 0 fwd bwd)

    go (sa:sas) ss _ (M2S_NoWriteAddress :- fwd) ~(b :- bwd) =
      B.bimap (toStallAck M2S_NoWriteAddress b sa :-) (M2S_NoWriteAddress :-) (go sas ss 0 fwd bwd)

    go (_sa:sas) (s:ss) _ (f0 :- fwd) ~(b0 :- bwd) =
      let
        (f1, b1, s1) = case compare 0 s of
          LT -> (M2S_NoWriteAddress, S2M_WriteAddress False, pred s:ss)
          EQ -> (f0, b0, if _awready b0 then ss else s:ss)
          GT -> error ("Unexpected negative stall: " <> show s)
      in
        B.bimap (b1 :-) (f1 :-) (go sas s1 0 fwd bwd)

    stallAcks
      | stallAck == StallCycle = [minBound..maxBound] \\ [StallCycle]
      | otherwise = [stallAck]

    toStallAck :: M2S_WriteAddress addr -> S2M_WriteAddress -> StallAck -> S2M_WriteAddress
    toStallAck (M2S_WriteAddress {}) rdy = P.const rdy
    toStallAck M2S_NoWriteAddress ack = \case
      StallWithNack -> S2M_WriteAddress False
      StallWithAck -> S2M_WriteAddress True
      StallWithErrorX -> errorX "No defined ack"
      StallTransparently -> ack
      StallCycle -> error "This function should not have been called with StallCycle."


instance (KnownDomain dom, KnownNat (Width addr)) =>
  H.Test (Axi4LiteWA dom addr) where
  expectToLengths Proxy lst = pure $ P.length lst

  expectN :: forall m .
    (HasCallStack, H.MonadTest m) =>
    Proxy (Axi4LiteWA dom addr) ->
    H.ExpectOptions ->
    Vec 1 Int ->
    [M2S_WriteAddress addr] ->
    m [M2S_WriteAddress addr]
  expectN Proxy (H.ExpectOptions{eoEmptyTail, eoTimeout}) (head -> nExpected) sampled = do
    go (Maybe.fromMaybe maxBound eoTimeout) nExpected sampled
    where
      catDatas [] = []
      catDatas (wa:was) = case wa of
        M2S_NoWriteAddress -> catDatas was
        _ -> wa : catDatas was

      go :: HasCallStack =>
        Int -> Int -> [M2S_WriteAddress addr] -> m [M2S_WriteAddress addr]
      go _ _ [] = error "unexpected end of signal."
      go _ 0 rest = do
        case catDatas (P.take eoEmptyTail rest) of
          [] -> pure (P.take nExpected $ catDatas sampled)
          superfluous ->
            let err = "Circuit produced more output than expected:" in
            H.failWith Nothing (err <> "\n\n" <> ppShow superfluous)
      go timeout n _ | timeout <= 0 =
        H.failWith Nothing $ P.concat
          [ "Circuit did not produce enough output. Expected "
          , show n, " more values. Sampled only ", show (nExpected - n), ":\n\n"
          , ppShow $ P.take (nExpected - n) (catDatas sampled) ]
      go timeout n (sample:as) = case sample of
        M2S_NoWriteAddress -> do go (pred timeout) n as
        M2S_WriteAddress {} -> go (Maybe.fromMaybe maxBound eoTimeout) (pred n) as

prop_axiFIFO :: H.Property
prop_axiFIFO =
  H.idWithModel
    -- (H.defExpectOptions { H.eoDriveEarly = True, H.eoTimeout = Just 30, H.eoEmptyTail = 30 })
    H.defExpectOptions
    genAxiFIFOInput
    id
    (exposeClockResetEnable (registerAxi @System) clockGen resetGen enableGen)

main :: IO ()
main = defaultMain $(testGroupGenerator)


identityAxi :: Circuit
  (Axi4LiteWA dom ('AddrWidth 4))
  (Axi4LiteWA dom ('AddrWidth 4))
identityAxi = Circuit $ (unbundle . (fmap go) . bundle)
  where
    go (a, b) = (b, a)


registerAxi :: HiddenClockResetEnable dom => Circuit
  (Axi4LiteWA dom ('AddrWidth 4))
  (Axi4LiteWA dom ('AddrWidth 4))
registerAxi = Circuit $ unbundle . (mealy go M2S_NoWriteAddress) . bundle
  where
    go state (lData, rAck) = (state', (S2M_WriteAddress lAck, state))
      where
        lAck = not lHasData || getAck rAck
        lHasData = case lData of
          M2S_NoWriteAddress -> False
          _ -> True
        getAck (S2M_WriteAddress b) = b

        state' = if lAck then lData else state



lhsStallC = stallWA (def {resetCycles = 30}) StallWithNack []
rhsStallC :: Circuit (Axi4LiteWA System ('AddrWidth 4)) (Axi4LiteWA System ('AddrWidth 4))
rhsStallC = stallWA (def {resetCycles = 30}) StallWithNack []
someDriver :: Circuit () (Axi4LiteWA System ('AddrWidth 4))
someDriver = driveC (def {resetCycles = 25}) [ M2S_WriteAddress { _awaddr = 0000 , _awprot = ( NotPrivileged , NonSecure , Data ) } ]
stalledAndDriven = someDriver |> lhsStallC |> exposedAxiFIFO |> rhsStallC

{-
1. Register ertussen gooien
2. dezelfde FIFO in Df maken.
-}

internals fifoLhsData = stallRhsData
  where
    (fifoLhsReady, fifoRhsData) = toSignals exposedAxiFIFO (fifoLhsData, stallLhsReady)
    (stallLhsReady, stallRhsData) = toSignals rhsStallC (fifoRhsData, pure $ S2M_WriteAddress False)



data WriteAddress (aw :: AddrWidth)
  = WriteAddress {
    _awaddr' :: !(BitVector (Width aw)),
    _awprot' :: PermissionsType 'KeepPermissions
  }

type Axi4LiteWA'
  (dom :: Domain)
  (aw :: AddrWidth) = Df dom (WriteAddress aw)


