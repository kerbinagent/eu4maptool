import BuildData
import Parser
import Codec.Picture
import ImageTracer
import Beijing

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
  let defMap = parseRDef g
  Right (ImageRGB8 bmp) <- readBitmap "resources/provinces.bmp"
  let allmap = buildShape defMap bmp
  putStrLn "%!PS\n1 setlinecap\n"
  mapM_ putStr result
  putStrLn "showpage"
