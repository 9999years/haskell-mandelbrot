module Graph where

import Codec.Picture

-- A range of numbers bounded by a minimum and a maximum
data Range a =
  Range a a
  deriving Show

-- the difference between the minimum and maximum of the range
rangespan :: (Num a) => Range a -> a
rangespan (Range min max) = max - min

-- the minimum of a range
rangemin (Range min max) = min
-- the maximum of a range
rangemax (Range min max) = max
-- the midpoint of a range
rangecenter (Range min max) = (min + max) / 2

data Point a =
  Point a a
  deriving Show

-- When rendering images, we frequently need to convert between coordinates
-- in the image raster and coordinates in the graph-space; by giving the
-- width and height of the rendered region as well as the pixel width of the
-- image, we can convert easily between the two coordinate-spaces
data GraphRegion a = GraphRegion
  { pxWidth :: Int
  , width   :: Range a
  , height  :: Range a
  } deriving Show

aspect (GraphRegion _ width height) = rangespan width / rangespan height

-- Given a GraphRegion, get its pixel height
pxHeight :: RealFloat a => GraphRegion a -> Int
pxHeight graph = round $ (fromIntegral $ pxWidth graph) * aspect graph

graphcenter (GraphRegion _ width height) =
  Point (rangecenter width) (rangecenter height)

-- Translates a point in the graph to a point on the image
graph2raster :: RealFloat a => GraphRegion a -> Point a -> Point Int
graph2raster graph (Point x y) =
  Point
    (pxW       * round ((x - rangemin w) / rangespan w))
    (pxH - pxH * round ((y - rangemin h) / rangespan h))
  where
    w = width graph
    h = height graph
    pxW = pxWidth graph
    pxH = pxHeight graph

-- Inverse of graph2raster; translates a point on the image to a point on the
-- graph
raster2graph :: RealFloat a => GraphRegion a -> Point Int -> Point a
raster2graph graph (Point x y) =
  let
    y' = pxHeight graph - y
    -- what the FUCK is going on here?
    convert pxAxis axisRange value =
      let range = axisRange graph
          span = rangespan range
          center = rangecenter range
          pxRange = pxAxis graph
      in span * fromIntegral value / fromIntegral pxRange
         + center - span / 2
  in Point
    (convert pxWidth  width  x)
    (convert pxHeight height y')

--
graph2image :: RealFloat a =>
    GraphRegion a -> (Int -> Int -> PixelRGB8)
    -> (Image PixelRGB8)
graph2image graph render = generateImage render width height
  where
    width = pxWidth graph
    height = pxHeight graph
