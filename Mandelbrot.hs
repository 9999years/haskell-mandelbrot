module Mandelbrot where

import Data.Maybe

import Codec.Picture
import Data.Complex

import Graph

-- i hate this!!!!
infinity :: RealFloat a => a
infinity = 1 / 0

-- is this value of c worth continuing to calculate?
-- NOTE: NaNs can sometimes arive in an Infinity - Infinity situation
cutoffC :: RealFloat a => Complex a -> Bool
cutoffC c = let m = magnitude c
            in m >= 2 || isNaN m

maybeCutoffC c =
  if cutoffC c
  then Nothing
  else Just c

z :: RealFloat a => Maybe (Complex a) -> Int -> Maybe (Complex a)
z Nothing n = Nothing
z c 0 = Just 0
z (Just c) n = (\z -> z ** 2 + c) <$> z (Just c) (n - 1) >>= maybeCutoffC

z' :: RealFloat a => Maybe (Complex a) -> Int -> Maybe (Complex a)
z' Nothing n = Nothing
z' c 0 = Just 1
z' (Just c) n =
  do zn  <- z  (Just c) (n - 1)
     z'n <- z' (Just c) (n - 1)
     maybeCutoffC $ 2 * zn * z'n + 1

-- see Quilez for derivation
distQuilez :: RealFloat a => Complex a -> Int -> a
distQuilez c n =
  fromMaybe infinity $
    do zn  <- magnitude <$> z  (Just c) n
       z'n <- magnitude <$> z' (Just c) n
       Just $ (zn * log zn) / z'n

distQuilez2 :: RealFloat a => Complex a -> Int -> a
distQuilez2 c n =
  fromMaybe infinity $
    do zn  <- magnitude <$> z  (Just c) n
       z'n <- magnitude <$> z' (Just c) n
       Just $ log (zn ** 2) * zn / z'n
