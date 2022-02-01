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

module NewDfTest where

import Prelude

import Protocols
import Clash.Prelude hiding (map, undefined, take)
import qualified Protocols.Df as Df

import qualified Protocols.Hedgehog as H
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import           Data.Default (Default)
import           Data.Kind (Type)




catMaybes :: Circuit (Df dom (Maybe a)) (Df dom a)
catMaybes = Circuit (unbundle . fmap go . bundle)
  where
    go (Df.NoData, _) = (Ack False, Df.NoData)
    go (Df.Data Nothing, _) = (Ack True, Df.NoData)
    go (Df.Data (Just d), Ack ack) = (Ack ack, Df.Data d)

testlist = [2,3,4,5,11,21,255,0,1]
sim = simulateCS top' testlist
sim' = take 200 $ simulateC top' def (map Df.Data testlist)
top' = exposeClockResetEnable (top @System) clockGen resetGen enableGen

top :: HiddenClockResetEnable dom =>
  Circuit (Df dom (Unsigned 8)) (Df dom (Unsigned 16))
top = slowSquarer |> (Df.map (+1))


slowSquarer :: HiddenClockResetEnable dom =>
  Circuit (Df dom (Unsigned 8)) (Df dom (Unsigned 16))
slowSquarer = Circuit (unbundle . (squarerM) . bundle)

squarerM :: HiddenClockResetEnable dom =>
  Signal dom (Df.Data (Unsigned 8), Ack) -> Signal dom (Ack, Df.Data (Unsigned 16))
squarerM = mealy squarer (0, 0, 0, Idle)

-- (partial result, left shifted version for addition, right shifted version for bitcheck, state)
type SquareState = (Unsigned 16, Unsigned 16, Unsigned 8, Phase)
data Phase = Idle | Squaring (Unsigned 3)
  deriving (Show, Generic, NFDataX)

squarer ::
  SquareState ->
  (Df.Data (Unsigned 8), Ack) ->
  (SquareState, (Ack, Df.Data (Unsigned 16)))
squarer (partial, shifted, bitcheck, phase) (n, ack) = (state', (ack', result))
  where
    state' = (partial', shifted', bitcheck', phase')
    (shifted', bitcheck') = case phase of
      Idle -> case n of
        (Df.Data n) -> (resize n, n)
        (Df.NoData) -> (0, 0)
      Squaring _ -> (shifted `shiftL` 1, bitcheck `shiftR` 1)

    partial' = case phase of
      Idle -> 0
      Squaring _ -> partial + addend

    addend = case phase of
      Idle -> 0
      Squaring _ -> if bitcheck .&. 1 == 1
        then shifted
        else 0

    ack' = case phase of
      Idle -> ack
      Squaring _ -> Ack False

    phase' = case phase of
      Idle -> case n of
        Df.Data n -> Squaring 7
        Df.NoData -> Idle
      Squaring 0 -> Idle
      Squaring n -> if bitcheck == 0
        then Squaring 0
        else Squaring (n - 1)

    result = case phase of
      Squaring 0 -> Df.Data partial'
      _ -> Df.NoData



dfDriver :: Circuit () (Df System Int)
dfDriver = driveC def [Df.Data 4]
