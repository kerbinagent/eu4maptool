module Drawing where
import MapType
import qualified Data.Map as Map

rectIntersect :: (Ord a, Integral a) => ((a,a),(a,a)) -> ((a,a),(a,a)) -> Bool
rectIntersect ((a,b),(c,d)) ((a',b'),(c',d')) = not $ a'>c || c'<a || b'>d || d'<b

inRangeProv :: RangeMap -> (Vertice, Vertice) -> [ProvID]
inRangeProv rmap r = [fst x | x <- Map.toList rmap, snd x `rectIntersect` r ]
