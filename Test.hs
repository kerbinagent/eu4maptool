import BuildData
import Parser
import Codec.Picture
import ImageTracer
import Beijing
import qualified Data.Map as Map

bezierToPS :: [(Double, Double)] -> String
bezierToPS = drawps . drawBezier where
  drawps [] = ""
  drawps (((x,y),(x',y')):ps) = "newpath\n" ++ show x ++ " " ++ show y ++ " moveto\n" ++ show x' ++ " " ++ show y' ++ " lineto\nstroke\n" ++ drawps ps

result :: [String]
result = (map bezierToPS . getBezierControl) newbj where
  newbj = map (\(a,b) -> (a-4500,b-500)) (optimalPolygon beijing)

main :: IO ()
main = do
  g <- readFile "resources/definition.csv"
  c <- readFile "resources/vanilla"
  let defMap = parseRDef g
  let lumap = Map.fromAscList $ map read (lines c)
  Right (ImageRGB8 bmp) <- readBitmap "resources/provinces.bmp"
  let smap = buildShape defMap bmp
  print $ provBezier smap lumap 18 
  --putStrLn "%!PS\n1 setlinecap\n"
  --mapM_ putStr result
  --putStrLn "showpage"
