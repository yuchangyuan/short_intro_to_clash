{-# language FlexibleInstances #-}
module Recip where

import Clash.Prelude
import Clash.Sized.Fixed
import Data.Ratio
import qualified Data.List as L
import qualified Prelude as P

class (RealFrac a, Integral (ScaleIndex a), NFDataX (ScaleIndex a)) => Scalable a where
  type ScaleIndex a :: Type
  type instance ScaleIndex a = Integer

  -- only for x > 1, n = 0 if x <= 1
  unscale :: a -> (a, ScaleIndex a)
  unscale x = let
    n = ceiling $ logBase 2 $ fromIntegral $ ceiling x
    y = x / (2 P.^ n)
    in (y, n)

  scale :: a -> (ScaleIndex a) -> a
  scale y n = if n > 0 then y * 2 P.^ n else y / 2 P.^ (-n)

  -- NOTE: define scaleReverse as "scale y (-n)" by default, but this not always work
  scaleReverse :: a -> (ScaleIndex a) -> a
  scaleReverse y n = scale y $ negate n

instance Scalable Float
instance Scalable Double
instance (Integral a, NFDataX a) => Scalable (Ratio a)

leadingZeroF :: (Num a) => (a, Bool) -> (a, Bool) -> (a, Bool)
leadingZeroF (leftIdx, leftAllZero) (rightIdx, rightAllZero) =
  if leftAllZero then (rightIdx, rightAllZero)
  else (leftIdx, False)

-- NOTE: for 0, return value is not correct
leadingZero :: (KnownNat n) => BitVector (n + 1) -> Index (n + 1)
leadingZero i = o where
  bv = zipWith (\idx bit -> (idx, not $ bitCoerce bit)) indicesI (bv2v i)
  o = fst $ fold leadingZeroF bv

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

  -- NOTE: here, scaleReverse y n != scale y (-n)
  scaleReverse y n = x where
    p = snatToNum (SNat :: SNat a)
    x = if n > p
        then y `shiftL` (fromIntegral $ n - p)
        else y `shiftR` (fromIntegral $ p - n)


recipStep d x = x * (2 - d * x)

recipI :: (Num a) => Int -> a -> a
recipI n d = L.foldr (.) id (L.replicate n $ recipStep d) $ 1

-- recipI 4 0.3 :: Double
-- recipI 6 0.3 :: Double
-- recipI 4 3 :: Double
-- recipI 6 3 :: Double

recipFull :: Scalable a => Int -> a -> a
recipFull k x = scaleReverse (recipI k y) n
  where
    (y, n) = unscale x

-- recipFull 6 0.7 :: Double
-- recipFull 6 0.7 :: UFixed 8 16
-- recipFull 6 (7 % 10)
-- recipFull 6 3 :: Double
-- recipFull 6 3 :: UFixed 8 16
-- recipFull 6 (3 % 1)

{-# ANN recip4_
  (Synthesize
    { t_name   = "recip4_"
    , t_inputs = [PortName "x"]
    , t_output = PortName "y"
    }) #-}
recip4_ :: UFixed 8 24 -> UFixed 8 24
recip4_ = recipI 4

{-# ANN recip4
  (Synthesize
    { t_name   = "recip4"
    , t_inputs = [PortName "x"]
    , t_output = PortName "y"
    }) #-}
recip4 :: UFixed 8 24 -> UFixed 8 24
recip4 = recipFull 4
