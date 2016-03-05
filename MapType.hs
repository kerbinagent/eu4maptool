-- type definitions and handy small functions
module MapType where
import Data.Word
import qualified Data.Map as Map
import Data.Array
type ColorRGB = (Word8,Word8,Word8)
type PixelPos = (Word16,Word16)
-- int position of intersections of pixels
type Vertice = (Word16,Word16)
type Edge = (Vertice, Vertice)
type Path = [Edge]
-- mapping province id to province name
type ProvID = Word16
type LocalMap = Map.Map ProvID String
type CountryLocal = Map.Map Word16 String
type DefMap = Map.Map ProvID ColorRGB
type ReverseDefMap = Map.Map ColorRGB Word16
type ShapeMap = Array (Word16,Word16) Word16
type RangeMap = Map.Map ProvID ((Word16, Word16), (Word16, Word16))
type ClosureMap = Map.Map ProvID Path
type AllProvince = Map.Map Word16 Province
data Direction = Up | Dn | Lf | Rg
  deriving (Show,Eq)
data Province = Province {
  getID::Int,
  getColor::ColorRGB,
  getSize::(PixelPos,PixelPos),
  getMapping::Map.Map PixelPos Bool
  }
  deriving (Show,Eq)

leftUp :: ShapeMap -> Word16 -> PixelPos
leftUp m p = head [(x,y) | x<- [1..j], y<- [1..k], m ! (x,y) == p] where
  (_,(j,k)) = bounds m

toVertice :: PixelPos -> [Vertice]
toVertice (x,y) = [(x-1,y-1),(x,y-1),(x-1,y),(x,y)]

edgeDirection :: Edge -> Direction
edgeDirection (a,b)
  | fst a == fst b = if snd b > snd a then Dn else Up
  | otherwise = if fst b > fst a then Rg else Lf

-- test if an edge satisfy that to the left is a province pixel and to the right is not
isEdge :: ShapeMap -> Edge -> Word16 -> Bool
isEdge m e p = (m ! px1 == p) && (m ! px2 /= p) where
  px1 = toPixel True e
  px2 = toPixel False e

edgeToEdge :: Edge -> [Edge]
edgeToEdge e@(_,(c,d)) = case edgeDirection e of
  Up -> [ ((c,d),(c+1,d)) , ((c,d),(c,d-1)) , ((c,d),(c-1,d)) ]
  Dn -> [ ((c,d),(c-1,d)) , ((c,d),(c,d+1)) , ((c,d),(c+1,d)) ]
  Lf -> [ ((c,d),(c,d-1)) , ((c,d),(c-1,d)) , ((c,d),(c,d+1)) ]
  Rg -> [ ((c,d),(c,d+1)) , ((c,d),(c+1,d)) , ((c,d),(c,d-1)) ]

-- given left (True) / right (False) -> get the corresponding pixel nexts to an edge
toPixel :: Bool -> Edge -> PixelPos
toPixel False (a,b) = toPixel True (b,a)
toPixel True e@(a,b) = case edgeDirection e of
  Up -> a
  Dn -> (fst b+1, snd b)
  Lf -> (fst a, snd a+1)
  Rg -> b
