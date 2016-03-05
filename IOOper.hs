module IOOper where
import System.Directory
import Data.Array
import Data.Word
import qualified Data.Map as Map
import Codec.Picture
import MapType
import HistoryType
import BuildData
import Parser
import ImageTracer

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

getpcmap :: FilePath -> IO ProvCountryMap
getpcmap dir = do
  ufs <- getDirectoryContents dir
  let fs = filter isProvHistory ufs
  buildPCMap dir fs

provPath :: FilePath
provPath = "resources/provinces/"

countryPath :: FilePath
countryPath = "resources/countries/"

initData :: IO (Map.Map ProvID [Vertice], RangeMap, ProvCountryMap, (Word16,Word16))
initData = do
  smap <- getsmap
  let (_,(i,j)) = bounds smap
  lumap <- getlumap
  drmap <- getrdmap
  pcmap <- getpcmap provPath
  let pmap = buildLongPath smap lumap drmap
  let rmap = buildRange pmap
  let plgmap = optimalPolygon <$> pmap
  return (plgmap, rmap, pcmap, (i,j))
