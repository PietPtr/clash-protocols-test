module Loops where


import Clash.Prelude
import qualified Data.List as L

loopL :: Int -> (Bool, Int) -> (Int, Int)
loopL state (enable, fromRight) = (state', state)
  where
    state' = if enable
      then fromRight
      else state

loopR :: Int -> Int -> (Int, Int)
loopR state fromLeft = (state', (state))
  where
    state' = if even fromLeft
      then fromLeft
      else state + 1


top enable = fromRight
  where
    l = mealy loopL 0
    r = mealy loopR 1

    fromLeft = l $ bundle (enable, fromRight)
    fromRight = r fromLeft

top' enable = fromRight
  where
    fromLeft = (\en r l -> if en then r else l) <$> enable <*> fromRight <*> leftReg
    leftReg = register 0 fromLeft
    fromRight = register 1 fromLeft'
    fromLeft' = (\l r -> if even l then l else r) <$> fromLeft <*> fromRight
