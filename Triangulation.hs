-- ear-clipping algorithm
-- assuming input is simple-polygon (a list of anti-clockwise consecutive points)
-- output list of triangles (a triangle is a list of three points)
-- the way we check if an ear is inside a polygon is rigorous, but it should work fine for
-- our purpose
module Triangulation where
import Data.List
type Point = (Float,Float)
type Vector = (Float,Float)
type Polygon = [Point]
type Triangle = Polygon


-- isAntiClockwise takes in two non-zero vectors and check if the second vector
-- is rotated anticlockwised by the first vector
-- when second vector is at angle pi to first vector, the function gives True
-- the input vector is never zero by our assumption of simple polygon
isAntiClockwise :: Vector->Vector->Bool
isAntiClockwise (x1,y1) (x2,y2)
                |x1*y2-x2*y1>=0 = True
                |otherwise = False


-- isInsidePolygon takes consecutive three points of a polygon and check
-- if the "ear" formed by these three points is inside the polygon
-- this check is not rigorous
isInsidePolygon :: Point->Point->Point->Polygon->Bool
isInsidePolygon (x0,y0) (x1,y1) (x2,y2) _ = isAntiClockwise (x1-x0,y1-y0) (x2-x0,y2-y0)

-- auxiliary function: rotating a list by moveing first element to last
listRot :: [a]->[a]
listRot [] = []
listRot (x:xs) = xs++[x]


-- once getEar detects a triangle ear that can be truncated off the polygon
-- it returns the triangle and the truncated polygon
getEar :: Polygon->(Triangle,Polygon)
getEar polygon = case polygon of
                  []->([],[])
                  [_]->([],[])
                  [_,_]->([],[])
                  p1:p2:p3:ps->if isInsidePolygon p1 p2 p3 polygon then ([p1,p2,p3],listRot(p1:p3:ps)) else getEar$listRot polygon


triangulate :: Polygon->[Triangle]
triangulate input = if length input <= 3 then [input] else if windingNumber input == (-1) then triangulate (reverse input) else triangle:triangulate polygon where
  triangle = fst (getEar input)
  polygon = snd (getEar input)
-----------------------------------------
subtrv (x1,y1) (x0,y0) = (x1-x0,y1-y0)
getVectors :: Polygon->[Vector]
getVectors polygon = zipWith subtrv (listRot polygon) polygon

getAngle :: Vector->Vector->Float
getAngle (x1,y1) (x0,y0)
  |u>=0 = theta
  |u<0 && v>=0 = pi-theta
  |u<0 && v<0 = -pi-theta
  where
    theta = asin (v/sqrt(u^2+v^2))
    u = x1*x0+y1*y0
    v = x0*y1-x1*y0

getAngles :: [Vector]->[Float]
getAngles vectors = zipWith getAngle (listRot vectors) vectors

windingNumber :: Polygon->Int
windingNumber = round.(/(2*pi)).sum.getAngles.delete (0.0,0.0).getVectors
------------------------------
--p0,p1,p2,p3 :: Point
--p0=(0,0)
--p1=(1,0)
--p2=(1,1)
--p3=(0,1)
--polygon=[p0,p1,p2,p3]
