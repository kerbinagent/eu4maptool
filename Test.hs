import BuildData
import Parser
import Codec.Picture
import Data.Array
import ImageTracer
import Beijing
import MapType
import qualified Data.Map.Strict as Map
import Data.Maybe

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
