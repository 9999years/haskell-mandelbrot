module Graph where

import Codec.Picture

-- functions for managing a graph
-- A range of numbers bounded by a minimum and a maximum
data Range a =
  Range a
        a

rangespan :: (Num a) => Range a -> a
rangespan (Range min max) = max - min

rangemin (Range min max) = min
rangemax (Range min max) = max
rangecenter (Range min max) = (min + max) / 2

data Point a =
  Point a
        a

data GraphRegion = GraphRegion
  { pxWidth :: Int
  , width :: Range Double
  , height :: Range Double
  }

aspect (GraphRegion _ width height) = rangespan width / rangespan height

-- Given a GraphRegion, get its pixel height
pxHeight :: GraphRegion -> Int
pxHeight graph = round $ (fromIntegral $ pxWidth graph) * aspect graph

graphcenter (GraphRegion _ width height) =
  Point (rangecenter width) (rangecenter height)

-- Translates a point in the graph to a point on the image
graph2raster :: GraphRegion -> Point Double -> Point Int
graph2raster graph (Point x y) =
  Point
    (pxW       * round ((x - rangemin w) / rangespan w))
    (pxH - pxH * round ((y - rangemin h) / rangespan h))
  where
    w = width graph
    h = height graph
    pxW = pxWidth graph
    pxH = pxHeight graph

-- Inverse of graph2raster
raster2graph :: GraphRegion -> Point Int -> Point Double
raster2graph graph (Point x y) =
  Point
    ((fromIntegral x / fromIntegral (pxWidth graph)) * rangespan (width graph)
     + rangecenter (width graph) -
     (rangespan (width graph)) / 2)
    ((fromIntegral y' / fromIntegral (pxHeight graph)) * rangespan (height graph)
     + rangecenter (height graph) -
     (rangespan (height graph)) / 2)
  where
    y' = pxHeight graph - y

graph2image :: (Int -> Int -> PixelRGB8) -> GraphRegion -> (Image PixelRGB8)
graph2image render graph = generateImage render (pxWidth graph) (pxHeight graph)
