-- build various data structures for further use
module BuildData where
import MapType
import Codec.Picture
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Word
import Data.Array

getProvince :: ReverseDefMap -> Image PixelRGB8 -> PixelPos -> Word16
getProvince dmap bmp (x,y) = let PixelRGB8 r g b = pixelAt bmp ((fromIntegral . toInteger) x) ((fromIntegral . toInteger) y) in fromMaybe 0 $ (r,g,b) `Map.lookup` dmap

buildShape :: ReverseDefMap -> Image PixelRGB8 -> ShapeMap
buildShape dmap bmp = array ((1,1),(m,n)) [((x,y),p) | x <- [1..m], y<- [1..n], let p = getProvince dmap bmp (x,y)] where
  m = fromIntegral $ imageWidth bmp - 1
  n = fromIntegral $ imageHeight bmp - 1
