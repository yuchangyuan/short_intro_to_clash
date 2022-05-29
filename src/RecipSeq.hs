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
            SS a -> SS a
recipSeq in_data = out_data
  where
    -- unscale in_data to y & n
    (y', n') = unbundle $ unscale <$> in_data

    n = register 0 n'
    y = register 0 y'

    out_data = register 0 $ scaleReverse <$> y <*> n

{-# ANN recipSeqTop
  (Synthesize
    { t_name   = "recip_seq_top"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortName "in_data"
                 ]
    , t_output = PortName "out_data"
    }) #-}
recipSeqTop :: Clock System -> Reset System ->
               SS (UFixed 8 24) -> SS (UFixed 8 24)
recipSeqTop clk rst = withClockResetEnable clk rst enableGen recipSeq
