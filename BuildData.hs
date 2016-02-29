-- build various data structures for further use
module BuildData where
-- using strict IO because too many files in province directory
-- will run out of file handles if not using strict IO
import qualified System.IO.Strict as STIO
import Control.Exception (evaluate)
import Control.Monad.State
import MapType
import Codec.Picture
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Word
import Data.Array
import Data.Char
import Parser
import HistoryType
import ImageTracer

getProvince :: ReverseDefMap -> Image PixelRGB8 -> Image PixelRGB8 -> PixelPos -> Word16
getProvince dmap bmp tbmp (x,y) = let PixelRGB8 r g b = pixelAt bmp ((fromIntegral . toInteger) x) ((fromIntegral . toInteger) y)
                                      PixelRGB8 r' g' b' = pixelAt tbmp ((fromIntegral . toInteger) x) ((fromIntegral . toInteger) y) in
  if (r',g',b') == (8,31,130) || (r',g',b') == (55,90,220) then 0 else fromMaybe 0 $ (r,g,b) `Map.lookup` dmap

buildShape :: ReverseDefMap -> Image PixelRGB8 -> Image PixelRGB8 -> ShapeMap
buildShape dmap bmp tbmp = array ((1,1),(m,n)) [((x,y),p) | x <- [1..m], y<- [1..n], let p = getProvince dmap bmp tbmp (x,y)] where
  m = fromIntegral $ imageWidth bmp - 1
  n = fromIntegral $ imageHeight bmp - 1

buildProvLocal :: [FilePath] -> LocalMap
buildProvLocal fs = Map.fromList $ map fpToProv (filter isProvHistory fs)

getPCPair :: String -> FilePath -> IO (Word16, Word16)
getPCPair dir f = do
  g <- STIO.readFile (dir++f)
  -- hash three letter country code into Word16 int
  let hashC cs = sum $ zipWith (\a b -> (ord a - ord 'A') * b) cs [1,26,676]
  ph <- evaluate $ buildProvince (fst $ fpToProv f) (parseHistory g)
  return (provinceID ph , fromIntegral $ (hashC . fst . head . controllers) ph)

buildPCMap :: String -> [FilePath] -> IO ProvCountryMap
buildPCMap dir fs = mapM (getPCPair dir) fs >>= return . Map.fromList

buildLeftUpMap :: Map.Map Word16 Vertice -> Vertice -> ShapeMap -> Map.Map Word16 Vertice
buildLeftUpMap lumap v@(x,y) smap = if (x==i-1) && (y==j-1) then lumap else
  if Map.member (smap ! v) lumap then buildLeftUpMap lumap (nextv v) smap else buildLeftUpMap (Map.insert (smap ! v) v lumap) (nextv v) smap where
    (_,(i,j)) = bounds smap
    nextv (a,b) = if a==i-1 then (1,b+1) else (a+1,b)

buildRightDownMap :: Map.Map Word16 Vertice -> Vertice -> ShapeMap -> Map.Map Word16 Vertice
buildRightDownMap rdmap v@(x,y) smap = if (x==1) && (y==1) then rdmap else
  if Map.member (smap ! v) rdmap then buildRightDownMap rdmap (nextv v) smap else buildRightDownMap (Map.insert (smap ! v) v rdmap) (nextv v) smap where
    (_,(i,_)) = bounds smap
    nextv (a,b) = if a==1 then (i-1,b-1) else (a-1,b)

buildLongPath :: ShapeMap -> Map.Map Word16 Vertice -> Map.Map Word16 Vertice -> Map.Map Word16 Path
buildLongPath smap lumap rdmap  = Map.fromAscList result where
  getPath stp pid = execState (buildPath smap pid) [firstEdge stp]
  getAlt stp pid = execState (buildPath smap pid) [firstAltEdge stp]
  luPath pid = getPath (fromMaybe (0,0) (Map.lookup pid lumap)) pid
  rdPath pid = getAlt (fromMaybe (0,0) (Map.lookup pid rdmap)) pid
  longer pid = if length (luPath pid) < length (rdPath pid) then rdPath pid else luPath pid
  allp = map fst $ Map.toAscList lumap
  result = zip allp (map longer allp)

provBezier :: Map.Map Word16 Path -> Word16 -> [[(Double, Double)]]
provBezier pthmap pid = result where
  ps = fromMaybe [] $ Map.lookup pid pthmap
  result = (getBezierControl . optimalPolygon) ps
