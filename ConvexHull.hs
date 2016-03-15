module ConvexHull where
import qualified Triangulation as TRI
type Line = (TRI.Point, TRI.Point)

pointLineDist :: TRI.Point -> Line -> Float
pointLineDist (x,y) ((x0,y0),(x1,y1)) = (x0-x) * (y1-y) - (y0-y) * (x1-x)

furtherHull :: [TRI.Point] -> Line -> [TRI.Point]
furtherHull ps l@(a,b) = if length aboves < 2 then a:aboves else furtherHull aboves (a,far) ++ furtherHull aboves (far,b) where
  allDist = [ (pointLineDist p l, p) | p <- ps]
  aboves = map snd $ filter ((>0) . fst) allDist
  far = (snd . maximum) allDist

quickHull :: [TRI.Point] -> [TRI.Point]
quickHull [] = []
quickHull ps = furtherHull ps (minx, maxx) ++ furtherHull ps (maxx, minx) where
  minx = minimum ps
  maxx = maximum ps
