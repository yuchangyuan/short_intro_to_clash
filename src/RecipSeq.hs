{-# language FlexibleInstances, MultiWayIf, PartialTypeSignatures #-}
module RecipSeq where

import Clash.Prelude
import Clash.Sized.Fixed
import Data.Ratio
import Data.List as L
import Prelude as P

import Recip

type SS a = Signal System a

data State = IDLE | BUSY | DONE deriving (Eq, Show, Generic)
instance NFDataX State

-- input: in_data, in_esp, in_valid, out_ready
-- output: in_ready, out_data, out_valid
recipSeq :: (Scalable a, HiddenClockResetEnable System, NFDataX a) =>
            SS a -> SS a -> SS Bool -> SS Bool -> SS (Bool, a, Bool)
recipSeq in_data in_esp in_valid out_ready = bundle (in_ready, out_data, out_valid)
  where
    -- start signal, state from IDLE -> BUSY
    s_start = in_ready .&&. in_valid .&&. (state .==. pure IDLE)

    -- unscale in_data to y & n
    (y', n') = unbundle $ unscale <$> in_data

    n = regEn 0 s_start n'
    y = regEn 0 s_start y'

    -- register in_esp to esp
    esp = regEn 0 s_start in_esp

    -- result
    res  = regEn 0 (s_start .||. (state .==. pure BUSY)) res'
    res' = mux s_start 1.0 $ recipStep <$> y <*> res

    -- signal done
    s_done = (abs (res - res') .<. esp) .&&. (state .==. pure BUSY)

    -- output data, scaleReverse back from res
    out_data = regEn 0 s_done $ scaleReverse <$> res <*> n

    -- became idle
    s_idle = out_ready .&&. out_valid .&&. (state .==. pure DONE)

    -- state
    stateT st (s, d, i) = if | s -> BUSY
                             | d -> DONE
                             | i -> IDLE
                             | otherwise -> st

    state = moore stateT id IDLE $ bundle (s_start, s_done, s_idle)

    -- handshake signal
    in_ready  = state .==. pure IDLE
    out_valid = state .==. pure DONE

testRecipSeq :: (Scalable a, HiddenClockResetEnable System, NFDataX a) =>
                a -> a -> [(a, Bool)]
testRecipSeq in_data in_esp = sampleN 10 $ bundle (out_data, out_valid) where
  (in_ready, out_data, out_valid) = unbundle $
                                    recipSeq (pure in_data) (pure in_esp) in_valid (pure True)

  -- in valid, False, False, True, False ...
  in_valid = fromList $ [False, False, True] P.++ P.repeat False

{-# ANN recipSeqTop
  (Synthesize
    { t_name   = "recip_seq_top"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortName "in_data"
                 , PortName "in_esp"
                 , PortName "in_valid"
                 , PortName "out_ready"
                 ]
    , t_output = PortProduct "" [ PortName "in_ready"
                                , PortName "out_data"
                                , PortName "out_valid"
                                ]
    }) #-}
recipSeqTop :: Clock System -> Reset System ->
               SS (UFixed 8 24) -> SS (UFixed 8 24) -> SS Bool -> SS Bool ->
               SS (Bool, UFixed 8 24, Bool)
recipSeqTop clk rst = withClockResetEnable clk rst enableGen recipSeq
