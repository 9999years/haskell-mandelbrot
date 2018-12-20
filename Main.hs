module Main where

import Codec.Picture
import Data.Complex
import Mandelbrot
import Graph

render :: GraphRegion -> Int -> Int -> PixelRGB8
render graph x y = PixelRGB8 r g b
  where
    (Point graphX graphY) = raster2graph graph (Point x y)
    distance = 100 * dist (graphX :+ graphY) 10
    r = fromIntegral $ round distance
    g = r
    b = r

main = savePngImage "mandelbrot.png" img
  where
    graph = GraphRegion 500 (Range (-2) 1) (Range (-1.5) 1.5)
    img = ImageRGB8 $ graph2image (render graph) graph
