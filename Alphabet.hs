module Alphabet where
import qualified Graphics.Gloss.Interface.IO.Game as GS
import Data.List
import Triangulation
import ImageTracer (two)


-- auxiliary functions
dist::Point->Point->Float
dist (x0,y0) (x1,y1) = sqrt$ (x0-x1)^two+(y0-y1)^two

-- absoluteAngle takes a vector and output an angle in (-pi,pi]
-- the output angle is the anticlockwise angle the vector is rotated from x-axis
-- when input vector is zero, it outputs 0
absoluteAngle :: Vector->Float
absoluteAngle (u,v)
  |(u,v)==(0,0) = 0
  |u>=0 = asin (v/r)
  |u<0&&v>=0 = pi - asin (v/r)
  |otherwise = -pi - asin (v/r)
  where r=sqrt$u^two+v^two

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
getMid0::Polygon->Point
getMid0 polygon=(x/fromIntegral n,y/fromIntegral n) where
  x=sum $ map fst polygon
  y=sum $ map snd polygon
  n=length polygon
{-
getMid::Polygon->Point
getMid polygon=(0.5*(x1+x2),0.5*(y1+y2)) where
  (x0,y0)=getMid0 polygon
  (x1,y1)=getClosest polygon (x0,y0)
  (x2,y2)=getAntipode polygon (x1,y1)
-}
getMid::Polygon->Point
getMid polygon=((1/3)*(x1+x2+x3),(1/3)*(y1+y2+y3)) where
  (x0,y0)=getMid0 polygon
  (x1,y1)=getClosest polygon (x0,y0)
  (x2,y2)=getAntipode3 polygon (x1,y1)
  (x3,y3)=getAntipode3 polygon (x2,y2)


-- given a polygon edge and a point p0, get the point furthest on edge to p0
getFurthest::Polygon->Point->Point
getFurthest [x] _ = x
getFurthest (x:xs) p0
    |dist x p0 < dist y p0 = y
    |otherwise = x
    where y = getFurthest xs p0

-- given a polygon edge and a point p0, get the point closest on edge to p0
getClosest::Polygon->Point->Point
getClosest [x] _ = x
getClosest (x:xs) p0
    |dist x p0 > dist y p0 = y
    |otherwise = x
    where y = getClosest xs p0

-- given a polygon edge and a point p0 inside edge, get the antipode of p0
getAntipode::Polygon->Point->Point
getAntipode polygon p0 = polygon!!(index `mod` (length polygon)) where
  index = quot (length polygon) 2 + head (elemIndices p0 polygon)

getAntipode3::Polygon->Point->Point
getAntipode3 polygon p0 = polygon!!(index `mod` (length polygon)) where
  index = quot (length polygon) 3 + head (elemIndices p0 polygon)

-- getOutline outputs (mid,furthest,antipode) of a polygon
getOutline::Polygon->(Point,Point,Point)
getOutline polygon=(mid,furthest,antipode) where
  mid=getMid polygon
  furthest=getFurthest polygon mid
  antipode=getAntipode polygon furthest

-- check if the center is approximately on line joining furthest point and its antipode
isColinear :: Point->Point->Point->Bool
isColinear (x0,y0) (x1,y1) (x2,y2) = if distance <=0.05*dist (x1,y1) (x2,y2) then True else False where
  distance = abs$((y2-y1)*x0-(x2-x1)*y0+x2*y1-y2*x1)/(sqrt((y2-y1)^two+(x2-x1)^two))

-- if the three points are not colinear, get the center of circle passing through them
getCenterRad :: Point->Point->Point->(Point,Float)
getCenterRad p0@(x0,y0) p1@(x1,y1) p2@(x2,y2) = (center,radius) where
  radius=abs$(dist p0 p1)*(dist p1 p2)*(dist p2 p0)*0.5/((x1-x0)*(y2-y0)-(x2-x0)*(y1-y0))
  center = intersectLine line1 line2
  line1 = (x1-x0,y1-y0,(-0.5)*(x1^two-x0^two+y1^two-y0^two))
  line2 = (x2-x0,y2-y0,(-0.5)*(x2^two-x0^two+y2^two-y0^two))

-- takes a polygon and number of alphabets and output list of
-- (position,angle), where angle represent the angle(in degree)
-- the alphabet is rotated clockwisely from upright
drawAlphabet :: Polygon->Int->([(Point,Float)],Float)
drawAlphabet polygon n
  |isColinear mid furthest antipode = drawLine furthest antipode n
  |otherwise = drawArc center mid furthest antipode radius n
  where
    mid = getMid polygon
    furthest = getFurthest polygon mid
    antipode = getAntipode polygon furthest
    (center,radius) = getCenterRad mid furthest antipode

drawLine :: Point->Point->Int->([(Point,Float)],Float)
drawLine (x1,y1) (x2,y2) n
  |n<=0 = ([],0)
  |otherwise = (map ((\j->((leftX + stepX * j,leftY + stepY * j),angle)).fromIntegral) [2..n+1], len/fromIntegral (n+3))
  where
    (leftX,leftY,rightX,rightY) = if x1<x2 then (x1,y1,x2,y2) else (x2,y2,x1,y1)
    len = dist (x1,y1) (x2,y2)
    stepX = (rightX - leftX)/(fromIntegral (n+3))
    stepY = (rightY - leftY)/(fromIntegral (n+3))
    angle = (180/pi)*(asin ((leftY - rightY)/len))


drawArc :: Point->Point->Point->Point->Float->Int->([(Point,Float)],Float)
drawArc (xc,yc) (_,y0) (x1,y1) (x2,y2) r n
-- center of circle above center of the word, alphabet pointing inward, alphabets increase angle
  |y0<=yc = ((map (\angle->(getPosition (xc,yc) r angle, convertAngle (angle-pi)))).(map ((\j->(startAngle + stepAngle * j)).fromIntegral))$[2..n+1],stepAngle*r)
-- center of circle below center of the word, alphabet pointing inward, alphabets decrease angle
  |otherwise = ((map (\angle->(getPosition (xc,yc) r angle, convertAngle angle))).(map ((\j->(startAngle - stepAngle * j)).fromIntegral))$[2..n+1], stepAngle*r)
  where
    (leftX,leftY,rightX,rightY) = if x1<x2 then (x1,y1,x2,y2) else (x2,y2,x1,y1)
    startAngle = absoluteAngle (leftX-xc,leftY-yc)
    deltaAngle = abs$ getAngle (leftX-xc,leftY-yc) (rightX-xc,rightY-yc)
    stepAngle = deltaAngle/(fromIntegral n+3)
    getPosition (x',y') r' angle = (x'+r'*cos angle,y'+r'*sin angle)

{-
--testDrawArc :: (Point,Point,Point,Point,Float,Int)->[(Point,Float)]
testDrawArc ((xc,yc),(x0,y0),(x1,y1),(x2,y2),r,n)=(map (\angle->(getPosition (xc,yc) r angle))) $ (map ((\j->(startAngle + stepAngle * j)).fromIntegral))$[1..n]
-- center of circle above center of the word, alphabet pointing inward, alphabets increase angle
--  |y0<=yc = (map (\angle->(getPosition (xc,yc) r angle, -convertAngle angle))).(map ((\j->(startAngle + stepAngle * j)).fromIntegral))$[1..n]
-- center of circle below center of the word, alphabet pointing inward, alphabets decrease angle
--  |y0>yc = (map (\angle->(getPosition (xc,yc) r angle, convertAngle angle))).(map ((\j->(startAngle - stepAngle * j)).fromIntegral))$[1..n]
  where
    (leftX,leftY,rightX,rightY) = if x1<x2 then (x1,y1,x2,y2) else (x2,y2,x1,y1)
    startAngle = absoluteAngle (leftX-xc,leftY-yc)
    deltaAngle = abs$ getAngle (leftX-xc,leftY-yc) (rightX-xc,rightY-yc)
    stepAngle = deltaAngle/(fromIntegral n+1)
    getPosition (x',y') r' angle = (x'+r'*cos angle,y'+r'*sin angle)
testCircle::(Point,Point,Point,Point,Float,Int)
testCircle=((0.0,0.0),(0.0,-2.0),(-1.879,-0.684),(1.532,-1.286),2.0,7)
-}

-- convertAngle receives angle decribing position of the alphabet convert it into
-- the orientation of the outwardly pointing alphabet (in degree and clockwise from y-axis)
convertAngle :: Float->Float
convertAngle radian = (180/pi)*(0.5*pi-radian)

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
  (positions, zz) = drawAlphabet ps l
  renderchar z c p = uncurry GS.translate (fst p) $ GS.scale (z*0.001*zz) (z*0.001*zz) $ GS.rotate (snd p) $ GS.text c
