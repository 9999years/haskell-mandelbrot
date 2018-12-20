module Mandelbrot where

import Codec.Picture
import Data.Complex
import Graph

magn :: RealFloat a => Complex a -> a
magn c = realPart $ abs c

-- the actual mandelbrot code...
z :: Complex Double -> Int -> Complex Double
z c 0 = 0
z c n = zn ** 2 + c
  where
    zn = z c (n - 1)

z' :: Complex Double -> Int -> Complex Double
z' c 0 = 1
z' c n = 2 * zn * z'n + 1
  where
    zn = z   c (n - 1)
    z'n = z' c (n - 1)

-- see Quilez for derivation
dist :: Complex Double -> Int -> Double
dist c n = (zn * log zn) / (z'n)
  where
    zn  = magn $ z  c n
    z'n = magn $ z' c n
