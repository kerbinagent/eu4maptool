module IOOper where
import System.Directory
import Data.Maybe (fromMaybe)
import Data.Array
import Data.Word
import qualified Data.Map.Strict as Map
import Codec.Picture
import MapType
import HistoryType
import BuildData
import Parser
import ImageTracer

getsmap :: IO ShapeMap
getsmap = do
  g <- readFile "resources/altdef.csv"
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

getprovlocal :: FilePath -> IO LocalMap
getprovlocal dir = getDirectoryContents dir >>= return . buildProvLocal

provPath :: FilePath
provPath = "resources/provinces/"

countryPath :: FilePath
countryPath = "resources/countries/"

countryPathConfig :: FilePath
countryPathConfig = "resources/00_countries.txt"

localCPath :: FilePath
localCPath = "resources/countries_l_english.yml"

getcountries :: FilePath -> FilePath -> FilePath -> IO CountryMap
getcountries f cp dir = do
  local <- readFile f >>= return . map oneLocal . filter isLocal . lines
  fs <- getDirectoryContents dir
  -- drop 10 to eliminate "countries/" prefix
  cf <- readFile cp >>= return . filter (not . null . snd) . map ((\(_,_,_,a) -> if null a then (0,"") else (hashC (head a), drop 10 (last a))) . getPathConfig) . filter isPathConfig . lines
  colors <- buildColors dir $ filter isCountry fs
  let colormap = Map.fromList colors
  return $ Map.unions [Map.singleton (fst l) (uncurry Country l (fromMaybe [255,255,255] (Map.lookup (snd c) colormap))) | l <- local, c <- cf, fst l == fromIntegral (fst c) ]

initData :: IO (PolygonMap, RangeMap, LocalMap, ProvCountryMap, CountryMap, (Word16,Word16))
initData = do
  smap <- getsmap
  let (_,(i,j)) = bounds smap
  lumap <- getlumap
  drmap <- getrdmap
  pcmap <- getpcmap provPath
  lcmap <- getprovlocal provPath
  ctmap <- getcountries localCPath countryPathConfig countryPath
  let pmap = buildLongPath smap lumap drmap
  let rmap = buildRange pmap
  let plgmap = getBezierControl . optimalPolygon <$> pmap
  return (plgmap, rmap, lcmap, pcmap, ctmap, (i,j))
