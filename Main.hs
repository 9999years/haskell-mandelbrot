module Main where

import Codec.Picture
import Data.Complex
import Data.Time

import Mandelbrot
import Graph

-- this equation's always made my head hurt
clamp :: (Ord a) => a -> a -> a -> a
clamp min' max' n = min (max n min') max'

clampPixel :: (Integral a) => a -> Pixel8
clampPixel n = fromIntegral $ clamp 0 255 n

graphArea graph = gw * gh
  where
    gw = rangespan $ width graph
    gh = rangespan $ height graph

gray n = PixelRGB8 n' n' n'
  where
    n' = fromIntegral n

-- https://www.shadertoy.com/view/lsX3W4
-- gives a "color" from a distance and a graph
color :: (RealFloat a) => GraphRegion a
    -> a -- distance to color
    -> a -- color, between 0 and 1
color graph dist = d
  where
    gw = rangespan $ width graph
    d = dist / gw
    clampUnit = clamp 0 1

render :: RealFloat a => GraphRegion a -> Int -> Int -> PixelRGB8
render graph x y = gray $ ceiling distance
  where
    iterations = 64
    (Point graphX graphY) = raster2graph graph (Point x y)
    distance = distQuilez2 (graphX :+ graphY) iterations
    r = clampPixel $ round $ (fromIntegral x) / (100 * (color graph distance))

-- Formatter for use in filenames
dateTimeFormatter :: FormatTime t => t -> String
dateTimeFormatter = formatTime defaultTimeLocale format
  where
    format = iso8601DateFormat $ Just "%H_%M_%S"

filename t = "mandelbrot-" ++ dateTimeFormatter t ++ ".png"

imgCoords :: RealFloat a => GraphRegion a -> [[Point a]]
imgCoords graph =
  [[raster2graph graph $ Point x y | x <- [0..pxWidth graph]]
    | y <- [0..pxHeight graph]]

mandelbrotGraph pxWidth =
  let xRange = Range (-2) 1
      yRange = Range (-1.5) 1.5
  in GraphRegion pxWidth xRange yRange

main2 =
  do
    time <- getCurrentTime
    let file = filename time
    putStrLn file
    savePngImage file img
  where
    graph = mandelbrotGraph 500
    img = ImageRGB8 $ graph2image graph $ render graph

main =
  let iterations = 16
      graph = mandelbrotGraph 500
      dist (Point x y) = distQuilez2 (x :+ y) iterations
      fmap2d = fmap . fmap
      distances =
        --unlines $
        fmap unwords -- join strings by space and then by line
        $ fmap2d show -- Double -> String
        $ fmap2d dist -- get distance for each pt
        $ imgCoords graph
  in do
    --putStrLn "Writing..."
    --writeFile "distances.txt" distances
    putStrLn $ distances !! 0
