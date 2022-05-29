{-# language FlexibleInstances #-}
module Test where

import Clash.Prelude
import Clash.Sized.Fixed

leadingZeroF :: (Num a) => (a, Bool) -> (a, Bool) -> (a, Bool)
leadingZeroF (leftIdx, leftAllZero) (rightIdx, rightAllZero) =
  if leftAllZero then (rightIdx, rightAllZero)
  else (leftIdx, False)

-- NOTE: for 0, return value is not correct
leadingZero :: (KnownNat n) => BitVector (n + 1) -> Index (n + 1)
leadingZero i = o where
  bv = zipWith (\idx bit -> (idx, not $ bitCoerce bit)) indicesI (bv2v i)
  o = fst $ fold leadingZeroF bv

class (RealFrac a, Integral (ScaleIndex a), NFDataX (ScaleIndex a)) => Scalable a where
  type ScaleIndex a :: Type
  unscale :: a -> (a, ScaleIndex a)
  scale :: a -> (ScaleIndex a) -> a

instance (KnownNat a, KnownNat b, a ~ (n + 1)) => Scalable (UFixed a b) where
  type ScaleIndex (UFixed a b) = Index (a + b)

  unscale x = (y, n) where
    v = bitCoerce x
    n = leadingZero v
    p = snatToNum (SNat :: SNat a) -- length of integer part
    y = if n > p
        then x `shiftL` (fromIntegral $ n - p)
        else x `shiftR` (fromIntegral $ p - n)

  scale y n = x where
    p = snatToNum (SNat :: SNat a)
    x = if n > p
        then y `shiftR` (fromIntegral $ n - p)
        else y `shiftL` (fromIntegral $ p - n)


type SS a = Signal System a

test :: (Scalable a, HiddenClockResetEnable System, NFDataX a) =>
            SS a -> SS a
test in_data = out_data
  where
    -- unscale in_data to y & n
    (y', n') = unbundle $ unscale <$> in_data

    n = register 0 n'
    y = register 0 y'

    out_data = register 0 $ scale <$> y <*> n

{-# ANN testTop
  (Synthesize
    { t_name   = "test_top"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortName "in_data"
                 ]
    , t_output = PortName "out_data"
    }) #-}
testTop :: Clock System -> Reset System ->
           SS (UFixed 8 24) -> SS (UFixed 8 24)
testTop clk rst = withClockResetEnable clk rst enableGen test
