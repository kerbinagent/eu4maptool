module Alphabet where
type Point = (Float,Float)
type Vector = (Float,Float)
type Polygon = [Point]
type Triangle = Polygon
import Data.List
getMid::Polygon->Point
getCenter polygon=(x/n,y/n) where
  x=sum $ map fst polygon
  y=sum $ map snd polygon
  n=length polygon

dist::Point->Point->Float
dist (x0,y0) (x1,y1) = sqrt$ (x0-x1)^2+(y0-y1)^2

-- given a polygon edge and a point p0, get the point furthest on edge to p0
getFurthest::Polygon->Point->Point
getFurthest [x] p0 = x
getFurthest (x:xs) p0
    |dist x p0 < dist y p0 = y
    |otherwise = x
    where y = getFurthest qs p0

-- given a polygon edge and a point p0 inside edge, get the antipode of p0
getAntipode::Polygon->Point->Point
getAntipode polygon p0 = polygon!!(index `mod` (length polygon) where
  index=quot (length polygon) 2 + head (elemIndices p0 polygon)

-- check if the center is approximately on line joining furthest point and its antipode
isColinear :: Point->Point->Point->Bool

-- if three point are not colinear, get the center of circle passing through them
getCenterRad :: Point->Point->(Point,Float)

-- takes a polygon and number of alphabets and output list of
-- (position,angle), where angle represent the angle(in degree)
-- the alphabet is rotated clockwisely from upright
drawAlphabet :: Polygon->Int->[(Point,Float)]
drawAlphabet polygon n
  |isColinear mid furthest antipode = drawLine furthest mid n
  |otherwise = drawArc center furthest antipode rad n
  where
    mid = getMid polygon
    furthest = getFurthest polygon mid
    antipode = getAntipode polygon furthest
    (center,rad) = getCenterRad mid furthest antipode

drawLine :: Point->Point->Int->[(Point,Float)]
drawLine (x1,y1) (x2,y2) n
  |n<=0 = []
  |otherwise = map ((\j->((leftX + stepX * (j - 0.5),leftY + stepY * (j - 0.5)),angle)).fromIntegral) [1..n]
  where
    (leftX,leftY,rightX,rightY) = if x1<x2 then (x1,y1,x2,y2) else (x2,y2,x1,y1)
    len = sqrt $ (x1-x2)^2+(y1-y2)^2
    stepX = (rightX - leftX)/(fromIntegral n)
    stepY = (rightY - leftY)/(fromIntegral n)
    angle = asin $ (leftY - rightY)/len

drawArc :: Point->Point->Point->Float->Int->[(Point,Float)]
