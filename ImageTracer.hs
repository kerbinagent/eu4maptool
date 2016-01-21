module ImageTracer where
import MapType
import Codec.Picture
import Control.Monad.State
import qualified Data.Map as Map
import Data.Word

-- construct the first edge of a province from the upper left corner
firstEdge :: ShapeMap -> Word16 -> Edge
firstEdge m p = (a, b) where
  vs = toVertice $ leftUp m p
  a = head vs
  b = vs !! 2

getNextEdge :: ShapeMap -> Word16 -> State Path ()
getNextEdge m p = do
  es <- get
  let e@(_,v) = last es
      newV = head [x | x<- allEdge v, isEdge m x p, x/=e]
  put $ es++[newV]

buildPath :: ShapeMap -> Word16 -> State Path ()
buildPath m p = do
  getNextEdge m p
  es <- get
  if (fst . head) es /= (snd . last) es then buildPath m p else put es
