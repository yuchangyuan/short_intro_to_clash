{-# language FlexibleInstances #-}
module Recip where

import Clash.Prelude
import Clash.Sized.Fixed
import Data.Ratio
import Data.List as L
import Prelude as P

import Data.Ratio

class RealFrac a => Scalable a where
  unscale :: Integral b => a -> (a, b)
  unscale x = let
    n = ceiling $ logBase 2 $ fromIntegral $ ceiling x
    y = x / (2 P.^ n)
    in (y, n)

  scale :: Integral b => a -> b -> a
  scale y n = if n > 0 then y * 2 P.^ n else y / 2 P.^ (-n)

instance Scalable Float
instance Scalable Double
instance Integral a => Scalable (Ratio a)

instance (KnownNat a, KnownNat b) => Scalable (UFixed a b)

f d x = x * (2 - d * x)

recipI :: (Num a) => Int -> a -> a
recipI n d = L.foldr (.) id (L.replicate n $ f d) $ 1

-- recipI 4 0.3 :: Double
-- recipI 6 0.3 :: Double
-- recipI 4 3 :: Double
-- recipI 6 3 :: Double

recipFull :: (Scalable a) => Int -> a -> a
recipFull k x = scale (recipI k y) (-n)
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
