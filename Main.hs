import Data.Array
import Data.Word
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

getlumap :: IO (Map.Map ProvID Vertice)
getlumap = readFile "resources/vanillalu" >>= return . Map.fromAscList . map read . lines

getrdmap :: IO (Map.Map ProvID Vertice)
getrdmap = readFile "resources/vanillard" >>= return . Map.fromAscList . map read . lines

initData :: IO (Map.Map ProvID [Vertice], RangeMap, (Word16,Word16))
initData = do
  smap <- getsmap
  let (_,(i,j)) = bounds smap
  lumap <- getlumap
  drmap <- getrdmap
  let pmap = buildLongPath smap lumap drmap
  let rmap = buildRange pmap
  let plgmap = optimalPolygon <$> pmap
  return (plgmap, rmap, (i,j))

main :: IO ()
main = do
  (plgmap, rmap, (i,j)) <- initData
  GS.playIO
    (GS.InWindow "EUIV Map Editor" (1280,720) (0,0))
    GS.azure
    1
    (plgmap, rmap, (i,j), (1920,1080),constantinople, 4.5, False)
    renderWorld
    handleEvent
    stepWorld
