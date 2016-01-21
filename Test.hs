import BuildData
import Parser
import Codec.Picture

main :: IO ()
main = do
  g <- readFile "definition.csv"
  let defMap = parseRDef g
  Right (ImageRGB8 bmp) <- readBitmap "provinces.bmp"
  let allmap = altBuild defMap bmp
  print $ take 100 $ reverse allmap
