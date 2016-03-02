import BuildData
import Parser
import Codec.Picture
import Data.Array
import ImageTracer
import Beijing
import MapType
import qualified Data.Map as Map
import Data.Maybe

bezierToPS :: [(Double, Double)] -> String
bezierToPS = drawps . drawBezier where
  drawps [] = ""
  drawps (((x,y),(x',y')):ps) = "newpath\n" ++ show x ++ " " ++ show y ++ " moveto\n" ++ show x' ++ " " ++ show y' ++ " lineto\nstroke\n" ++ drawps ps

result :: [String]
result = (map bezierToPS . getBezierControl) newbj where
  newbj = map (\(a,b) -> (a-4500,b-500)) (optimalPolygon beijing)

getsmap :: IO ShapeMap
getsmap = do
  g <- readFile "resources/definition.csv"
  let defMap = parseRDef g
  Right (ImageRGB8 bmp) <- readBitmap "resources/provinces.bmp"
  s <- readFile "resources/sealist"
  let seamap = Map.fromList $ zip (map read (lines s)) (repeat True)
  let smap = buildShape defMap bmp seamap
  return smap

main :: IO ()
main = do
  c <- readFile "resources/vanillalu"
  b <- readFile "resources/vanillard"
  smap <- getsmap
  let (_,(i,j)) = bounds smap
  let lumap = Map.fromAscList $ map read (lines c)
  let drmap = Map.fromAscList $ map read (lines b)
  let pmap = buildLongPath smap lumap drmap
  --mapM_ print $ Map.toList drmap
  mapM_ print $ zip [1..3003] $ map (length . provBezier pmap) [1..3003]
  --putStrLn "%!PS\n1 setlinecap\n"
  --mapM_ putStr result
  --putStrLn "showpage"
