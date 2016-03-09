-- ear-clipping algorithm
-- assuming input is simple-polygon (a list of anti-clockwise consecutive points)
-- output list of triangles (a triangle is a list of three points)
-- the way we check if an ear is inside a polygon is rigorous, but it should work fine for
-- our purpose
module Triangulation where
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
                  p1:p2:p3:ps->if isInsidePolygon p1 p2 p3 polygon then ([p1,p2,p3],p1:p3:ps) else getEar$listRot polygon


triangulate :: Polygon->[Triangle]
triangulate input = if length input <= 3 then [input] else triangle:triangulate polygon where
  triangle = fst (getEar input)
  polygon = snd (getEar input)

-- test
p0,p1,p2,p3,p4,p5,p6,p7,p8,p9::Point
p0=(0,0)
p1=(2,0)
p2=(1,-2)
p3=(4,-2)
p4=(3,3)
p5=(0,1)
p6=(-3,3)
p7=(-4,-2)
p8=(-1,-2)
p9=(-2,0)
polygon::Polygon
polygon=[p0,p1,p2,p3,p4,p5,p6,p7,p8,p9]
