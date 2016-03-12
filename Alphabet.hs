module Alphabet where
import qualified Graphics.Gloss.Interface.IO.Game as GS
import Data.List
import Triangulation


-- auxiliary functions
dist::Point->Point->Float
dist (x0,y0) (x1,y1) = sqrt$ (x0-x1)^2+(y0-y1)^2

-- absoluteAngle takes a vector and output an angle in (-pi,pi]
-- the output angle is the anticlockwise angle the vector is rotated from x-axis
-- when input vector is zero, it outputs 0
absoluteAngle :: Vector->Float
absoluteAngle (u,v)
  |(u,v)==(0,0) = 0
  |u>=0 = asin (u/r)
  |u<0&&v>=0 = pi - asin (u/r)
  |u<0&&v<0 = -pi - asin (u/r)
  where r=sqrt$u^2+v^2

{-
-- stupidAngle takes a vector and output the angle this vector is rotated clockwisely from y-axis, in radian
-- the output is within [-0.5pi,1.5pi)
stupidAngle :: Vector->Float
stupidAngle vector = 0.5*pi - absoluteAngle vector
-}

-- intersectLine takes two lines (assume they do intersect) and outputs the intersection
intersectLine :: (Float,Float,Float)->(Float,Float,Float)->Point
intersectLine (a1,b1,c1) (a2,b2,c2) = ((-b2*c1+b1*c2)/(a1*b2-a2*b1),(a2*c1-a1*c2)/(a1*b2-a2*b1))

--------------------------------------------
getMid::Polygon->Point
getMid polygon=(x/fromIntegral n,y/fromIntegral n) where
  x=sum $ map fst polygon
  y=sum $ map snd polygon
  n=length polygon

-- given a polygon edge and a point p0, get the point furthest on edge to p0
getFurthest::Polygon->Point->Point
getFurthest [x] _ = x
getFurthest (x:xs) p0
    |dist x p0 < dist y p0 = y
    |otherwise = x
    where y = getFurthest xs p0

-- given a polygon edge and a point p0 inside edge, get the antipode of p0
getAntipode::Polygon->Point->Point
getAntipode polygon p0 = polygon!!(index `mod` (length polygon)) where
  index = quot (length polygon) 2 + head (elemIndices p0 polygon)

-- check if the center is approximately on line joining furthest point and its antipode
isColinear :: Point->Point->Point->Bool
isColinear (x0,y0) (x1,y1) (x2,y2) = if distance <=0.05*dist (x1,y1) (x2,y2) then True else False where
  distance = abs$((y2-y1)*x0-(x2-x1)*y0+x2*y1-y2*x1)/(sqrt((y2-y1)^2+(x2-x1)^2))

-- if the three points are not colinear, get the center of circle passing through them
getCenterRad :: Point->Point->Point->(Point,Float)
getCenterRad p0@(x0,y0) p1@(x1,y1) p2@(x2,y2) = (center,rad) where
  rad=(dist p0 p1)*(dist p1 p2)*(dist p2 p0)*0.5/((x1-x0)*(y2-y0)-(x2-x0)*(y1-y0))
  center = intersectLine line1 line2
  line1 = (x1-x0,y1-y0,(-0.5)*(x1^2-x0^2+y1^2-y0^2))
  line2 = (x2-x0,y2-y0,(-0.5)*(x2^2-x0^2+y2^2-y0^2))

-- takes a polygon and number of alphabets and output list of
-- (position,angle), where angle represent the angle(in degree)
-- the alphabet is rotated clockwisely from upright
drawAlphabet :: Polygon->Int->[(Point,Float)]
drawAlphabet polygon n
  |isColinear mid furthest antipode = drawLine furthest mid n
  |otherwise = drawArc center mid furthest antipode rad n
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
    len = dist (x1,y1) (x2,y2)
    stepX = (rightX - leftX)/(fromIntegral n)
    stepY = (rightY - leftY)/(fromIntegral n)
    angle = asin $ (leftY - rightY)/len


drawArc :: Point->Point->Point->Point->Float->Int->[(Point,Float)]
drawArc (xc,yc) (x0,y0) (x1,y1) (x2,y2) r n
-- center of circle above center of the word, alphabet pointing inward, alphabets increase angle
  |y0<=yc = (map (\angle->(getPosition (xc,yc) r angle, -convertAngle angle))).(map ((\j->(startAngle + deltaAngle * (j - 0.5))).fromIntegral))$[1..n]
-- center of circle below center of the word, alphabet pointing inward, alphabets decrease angle
  |y0>yc = (map (\angle->(getPosition (xc,yc) r angle, convertAngle angle))).(map ((\j->(startAngle - deltaAngle * (j - 0.5))).fromIntegral))$[1..n]
  where
    (leftX,leftY,rightX,rightY) = if x1<x2 then (x1,y1,x2,y2) else (x2,y2,x1,y1)
    startAngle = absoluteAngle (leftX-xc,leftY-yc)
    deltaAngle = abs$ getAngle (leftX-xc,leftY-yc) (rightX-xc,rightY-yc)
    getPosition (x',y') r' angle = (x'+r'*cos angle,y'+r'*sin angle)

-- convertAngle receives angle decribing position of the alphabet convert it into
-- the orientation of the outwardly pointing alphabet (in degree and clockwise from y-axis)
convertAngle :: Float->Float
convertAngle rad = (180/pi)*(0.5*pi-rad)

{-
-- getAlphaOrient takes center of circle and a point on circumference
-- and calculate the orientation of alphabet at that point
-- (clockse relative to y-axis, in degree)
-- if the boolean is true, the alphabet points outwards the circle
-- else the alphabet points inwards the circle
getAlphaOrient :: Point->Point->Bool->Float
getAlphaOrient (xc,yc) (x,y) bool
  |bool = stupidAngle (x-xc,y-yc) *180/pi
  |otherwise = stupidAngle (xc-x,yc-y) *180/pi
-}

renderName :: Float -> [Point] -> String -> GS.Picture
renderName zoom ps name = mconcat $ zipWith (renderchar zoom) chars positions where
  l = length name
  chars = map return name
  positions = drawAlphabet ps l
  renderchar z c p = GS.rotate (snd p) $ uncurry GS.translate (fst p) $ GS.scale z z $ GS.text c
