{-# language FlexibleInstances, MultiWayIf, PartialTypeSignatures #-}
module RecipSeq where

import Clash.Prelude
import Clash.Sized.Fixed
import Data.Ratio
import qualified Data.List as L
import qualified Prelude as P

import Recip

type SS a = Signal System a

recipSeq :: (Scalable a, HiddenClockResetEnable System, NFDataX a) =>
            SS a -> SS Bool -> SS a
recipSeq in_data in_valid = out_data
  where
    -- start signal, state from IDLE -> BUSY
    s_start = in_valid

    -- unscale in_data to y & n
    (y', n') = unbundle $ unscale <$> in_data

    n = regEn 0 s_start n'
    y = regEn 0 s_start y'

    s_done = register False s_start

    out_data = regEn 0 s_done $ scaleReverse <$> y <*> n

{-# ANN recipSeqTop
  (Synthesize
    { t_name   = "recip_seq_top"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortName "in_data"
                 , PortName "in_valid"
                 ]
    , t_output = PortName "out_data"
    }) #-}
recipSeqTop :: Clock System -> Reset System ->
               SS (UFixed 8 24) -> SS Bool ->
               SS (UFixed 8 24)
recipSeqTop clk rst = withClockResetEnable clk rst enableGen recipSeq
