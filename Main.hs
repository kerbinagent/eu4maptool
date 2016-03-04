import Data.Array
import qualified Data.Map as Map
import qualified Graphics.Gloss.Interface.IO.Game as GS
import Codec.Picture
import MapType
import BuildData
import Parser
import ImageTracer
import Drawing

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
  let rmap = buildRange pmap
  let plgmap = optimalPolygon <$> pmap
  GS.playIO
    (GS.InWindow "EUIV Map Editor" (1280,720) (0,0))
    GS.azure
    24
    (plgmap, rmap, (i,j), (1920,1080),constantinople, 5)
    renderWorld
    handleEvent
    stepWorld
