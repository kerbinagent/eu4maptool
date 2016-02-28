-- build various data structures for further use
module BuildData where
-- using strict IO because too many files in province directory
-- will run out of file handles if not using strict IO
import qualified System.IO.Strict as STIO
import Control.Exception (evaluate)
import MapType
import Codec.Picture
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Word
import Data.Array
import Data.Char
import Parser
import HistoryType

getProvince :: ReverseDefMap -> Image PixelRGB8 -> PixelPos -> Word16
getProvince dmap bmp (x,y) = let PixelRGB8 r g b = pixelAt bmp ((fromIntegral . toInteger) x) ((fromIntegral . toInteger) y) in fromMaybe 0 $ (r,g,b) `Map.lookup` dmap

buildShape :: ReverseDefMap -> Image PixelRGB8 -> ShapeMap
buildShape dmap bmp = array ((1,1),(m,n)) [((x,y),p) | x <- [1..m], y<- [1..n], let p = getProvince dmap bmp (x,y)] where
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
