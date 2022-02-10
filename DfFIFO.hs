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

module DfFIFO where

-- TODO: imports netter maken
import Clash.Prelude hiding (zip, undefined)
import           Clash.Signal.Internal (Signal(..))
import qualified Clash.Explicit.Prelude as CE
import Prelude hiding ((!!), replicate, head, length, take)
import qualified Prelude as P
import Protocols
import Protocols.Df
-- import Protocols.DfLike
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
fifoL = fifo `mealy` (0,0,replicate d4 (NoData))

type Elm  = Data Int
type Pntr n = Unsigned (n + 1)


dfFIFOlhs :: HiddenClockResetEnable dom =>
  Signal dom (Data Int, Bool) ->
  Signal dom (Ack, Bool, Elm)
dfFIFOlhs inp = outp
  where
    ~(lhsData, full) = unbundle inp
    outp = bundle (lhsReady, write, dataInFifo)
    lhsReady = (Ack . not) <$> full
    ~(write, dataInFifo) = unbundle $ fifoWriteSigs <$> full <*> lhsData

    fifoWriteSigs full lhsData = if full
      then (False, NoData)
      else case lhsData of
        NoData -> (False, NoData)
        w@(Data _) -> (True, w)

dfFIFOrhs :: HiddenClockResetEnable dom =>
  Signal dom (Ack, Bool, Elm) ->
  Signal dom (Data Int, Bool)
dfFIFOrhs inp = outp
  where
    ~(rhsReady, empty, dataOutFifo) = unbundle inp
    outp = bundle (rhsData, read)
    rhsData = dataSig <$> read <*> dataOutFifo
    read = readSig <$> rhsReady <*> empty

    -- Might be neater with the toBool function of a Df instance
    readSig ~(Ack b) empty = b && not empty
    dataSig read dataOutFifo = if read
      then dataOutFifo
      else NoData

dfFIFOCircuit :: HiddenClockResetEnable dom =>
  Circuit (Df dom Int) (Df dom Int)
dfFIFOCircuit = Circuit go
  where
    go ~(lhsData, rhsReady) = (lhsReady, rhsData)
      where
        ~(lhsReady, write, dataInFifo) = unbundle $ dfFIFOlhs $ bundle (lhsData, full)
        ~(rhsData, read) = unbundle $ dfFIFOrhs $ bundle (rhsReady, empty, dataOutFifo)
        ~(full, empty, dataOutFifo) = unbundle $ fifoL $ bundle (dataInFifo, write, read)

exposedDfFIFO :: Circuit
       (Df System Int)
       (Df System Int)
exposedDfFIFO =
  exposeClockResetEnable (dfFIFOCircuit) clockGen resetGen enableGen


genDfFIFOInput :: H.Gen [Int]
genDfFIFOInput =
  Gen.list (Range.linear 0 100) (genM2S (genInt 0 15))
  where
    genM2S genA = Gen.choice [genA]
    genInt a b = Gen.integral (Range.linear a b)



prop_axiFIFO :: H.Property
prop_axiFIFO =
  H.idWithModel
    -- (H.defExpectOptions { H.eoDriveEarly = True, H.eoTimeout = Just 30, H.eoEmptyTail = 30 })
    H.defExpectOptions
    genDfFIFOInput
    id
    -- exposedDfFIFO
    (exposeClockResetEnable (registerDf @System) clockGen resetGen enableGen)

main :: IO ()
main = defaultMain $(testGroupGenerator)


registerDf :: HiddenClockResetEnable dom => Circuit
  (Df dom Int) (Df dom Int)
registerDf = frs |>  (Circuit $ unbundle . (mealy go (NoData)) . bundle)
  where
    go state (lData, rAck) = (state', (Ack lAck, state))
      where
        lAck = not lHasData || getAck rAck
        lHasData = case state of
          NoData -> False
          _ -> True
        getAck (Ack b) = b

        state' = if lAck then lData else state

frs :: HiddenClockResetEnable dom =>
  Circuit (Df dom Int) (Df dom Int)
frs = Circuit (\(fwd, bwd) -> unbundle . fmap f . bundle $ (rstLow, fwd, bwd))
  where
    f (True, _, _) = (Ack False, NoData)
    f (False, fwd, bwd) = (bwd, fwd)
    rstLow = unsafeToHighPolarity hasReset
