module IOOper where
import System.Directory
import Data.List (isPrefixOf)
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

localCPath :: FilePath
localCPath = "resources/countries_l_english.yml"

colorException :: FilePath
colorException = "resources/cornersol"

getcountries :: FilePath -> FilePath -> IO CountryMap
getcountries f dir = do
  local <- readFile f >>= return . map oneLocal . filter isLocal . lines
  fs <- getDirectoryContents dir
  colors <- buildColors dir $ filter isCountry fs
  exceptions <- readFile colorException >>= return . map read . lines
  let exceptionmap = Map.fromList [(fst x, uncurry Country y (snd x)) | x<- exceptions, y <- local, fst x == fst y]
  return $ exceptionmap `Map.union` Map.unions [Map.singleton (fst l) (uncurry Country l (snd c)) | l <- local, c <- colors, snd l `isPrefixOf` fst c]

initData :: IO (PolygonMap, RangeMap, ProvCountryMap, CountryMap, (Word16,Word16))
initData = do
  smap <- getsmap
  let (_,(i,j)) = bounds smap
  lumap <- getlumap
  drmap <- getrdmap
  pcmap <- getpcmap provPath
  ctmap <- getcountries localCPath countryPath
  let pmap = buildLongPath smap lumap drmap
  let rmap = buildRange pmap
  let plgmap = optimalPolygon <$> pmap
  return (plgmap, rmap, pcmap, ctmap, (i,j))
