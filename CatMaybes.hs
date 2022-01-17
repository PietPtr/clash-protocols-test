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
{-# LANGUAGE ImlicitParams #-}

module CatMaybes where

import Protocols
import Clash.Prelude
import qualified Clash.Prelude as C
import qualified Protocols.Df as Df

import qualified Protocols.Hedgehog as H
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import           Data.Default (Default)
import           Data.Kind (Type)

import Test.Tasty
import Test.Tasty.TH (testGroupGenerator)
import Test.Tasty.Hedgehog.Extra (testProperty)

catMaybes :: Circuit (Df dom (Maybe a)) (Df dom a)
catMaybes = Circuit (unbundle . fmap go . bundle)
  where
    go (Df.NoData, _) = (Df.Ack False, Df.NoData)
    go (Df.Data Nothing, _) = (Df.Ack True, Df.NoData)
    go (Df.Data (Just d), Df.Ack ack) = (Df.Ack ack, Df.Data d)


mapMaybe :: (a -> Maybe b) -> Circuit (Df dom a) (Df dom b)
mapMaybe f = (Df.map f) |> (catMaybes)

mapMaybe' :: forall dom a b. (a -> Maybe b) -> Circuit (Df dom a) (Df dom b)
mapMaybe' f =
  Circuit (unbundle . fmap go . bundle)
  where
    go (Df.NoData, _) = (Df.Ack False, Df.NoData)
    go (Df.Data a, ~(Df.Ack ack)) = d a ack
    d a ack = case f a of
      (Just b) -> (Df.Ack ack, Df.Data b)
      Nothing -> (Df.Ack True, Df.NoData)


genCatMaybesInput :: H.Gen [Maybe Int]
genCatMaybesInput =
  Gen.list (Range.linear 0 100) (genMaybe (genInt 10 20))
  where
    genMaybe genA = Gen.choice [Gen.constant Nothing, Just <$> genA]

genInt a b = Gen.integral (Range.linear a b)

genMapMaybeInput :: H.Gen [Int]
genMapMaybeInput =
  Gen.list (Range.linear 0 100) (genInt (-50) 50)

prop_catMaybes :: H.Property
prop_catMaybes =
  H.idWithModel
    H.defExpectOptions
    genCatMaybesInput
    Maybe.catMaybes
    (catMaybes @System)

prop_mapMaybe :: H.Property
prop_mapMaybe =
  H.idWithModel
    H.defExpectOptions
    genMapMaybeInput
    (Maybe.mapMaybe testfunc)
    (mapMaybe' @System testfunc)

mapMaybeTest :: Circuit (Df dom Int) (Df dom Int)
mapMaybeTest = mapMaybe testfunc

testfunc :: Int -> Maybe Int
testfunc a = if a < 0 then Nothing else Just a

main :: IO ()
main = defaultMain $(testGroupGenerator)


in_list' :: [(Df.Data (Maybe (Unsigned 8)), Df.Ack (Unsigned 8))]
in_list' = List.zip in_list (List.repeat $ Df.Ack True)

in_list :: [Df.Data (Maybe (Unsigned 8))]
in_list = [
    Df.Data (Just 4),
    Df.Data (Just 5),
    Df.Data Nothing,
    Df.Data (Just 6)
  ]

sim = filter (/= Df.NoData) $ simulateC (catMaybes @System @(Unsigned 8)) def in_list

nonConform :: Circuit (Df dom a) (Df dom a)
nonConform = Circuit (unbundle . fmap go . bundle)
  where
    go (Df.NoData, _) = (Df.Ack False, Df.NoData)
    go (Df.Data d, _) = (Df.Ack False, Df.Data d)

resend_sim = simulateC (nonConform @System @(Unsigned 8)) def
  [Df.Data 1, Df.Data 2, Df.Data 3]

data MyDf (dom :: Domain) (a :: Type)

instance Protocol (MyDf dom a) where
  type Fwd (MyDf dom a) = Signal dom (MyData a)
  type Bwd (MyDf dom a) = Signal dom (MyAck a)


data MyData a = MyNoData | MyData !a
  deriving (Show, NFDataX, Generic)

newtype MyAck a = MyAck Bool
  deriving (Show, NFDataX, Generic)

instance Default (MyAck a) where
  def = MyAck True


catMaybes' :: Circuit (MyDf dom (Maybe a)) (MyDf dom a)
catMaybes' = Circuit (unbundle . fmap go . bundle)
  where
    go (MyNoData, _) = (MyAck False, MyNoData)
    go (MyData Nothing, _) = (MyAck True, MyNoData)
    go (MyData (Just d), MyAck ack) = (MyAck ack, MyData d)

system = bundle . toSignals catMaybes' . unbundle



-- since we work on only one circuit, I'm guessing no dataflow stuff is happening once converted to signals
-- perhaps we must connect a fast and slow component?
my_inlist :: [(MyData (Maybe (Unsigned 8)), MyAck (Unsigned 8))]
my_inlist = [
    (MyData $ Just 4, MyAck True),
    (MyData $ Nothing, MyAck True),
    (MyNoData, MyAck False),
    (MyNoData, MyAck False),
    (MyNoData, MyAck False)
  ]
