module FIR where

import Clash.Prelude

fir coeffs x = dotp coeffs (window x)
  where
    dotp as bs = sum (zipWith (*) as bs)

{-# ANN fir3u8
  (Synthesize
    { t_name   = "fir3u8"
    , t_inputs = [PortName "in"]
    , t_output = PortName "out"
    }) #-}
fir3u8 :: (HiddenClockResetEnable System) => Signal System (Unsigned 8) -> Signal System (Unsigned 8)
fir3u8 = fir (3 :> 4 :> 5 :> Nil)
