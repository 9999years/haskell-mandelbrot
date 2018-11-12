module Mandelbrot where

import Data.Complex

z :: Int -> Complex Double -> Complex Double
z 0 c = c
z n c = (z (n - 1) c) ** 2 + c

z' :: Int -> Complex Double -> Complex Double
z' 0 c = 0
z' n c = 2 * z (n - 1) c * z' (n - 1) c + 1

green :: Int -> Complex Double -> Complex Double
green n c = (log (z n c)) / 2^n

green' :: Int -> Complex Double -> Complex Double
green' n c = z' n c / (2^n * z n c)

dist :: Int -> Complex Double -> Complex Double
dist n c = green n c / green' n c

main = show (dist 0 0)
