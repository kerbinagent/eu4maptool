module ImageTracer where
import MapType
import Control.Monad.State
import Data.Word
import Data.List (nub, minimumBy)

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

-- cutpath by modulo value
cutPath :: [a] -> Int -> Int -> [a]
cutPath xs i j = let
                  j'= j+length xs
                  lxs = concat $ repeat xs
                 in
                  if i<=j then take (j-i+1) (snd $ splitAt i xs) else take (j'-i+1) (snd $ splitAt i lxs)

-- build squares around vertices
buildSquare :: Vertice -> [(Double,Double)]
buildSquare (a,b) = [(fromIntegral a-0.5,fromIntegral b-0.5),(fromIntegral a+0.5,fromIntegral b-0.5),(fromIntegral a-0.5,fromIntegral b+0.5),(fromIntegral a+0.5,fromIntegral b-0.5)]

-- given a point of departure, calculate the range of inclination of line that can intersects a given square (expressed as a list of vertices
rangeSquare :: (Double,Double) -> [(Double,Double)] -> (Double,Double)
rangeSquare p es = (minimum rg, maximum rg) where
  rg = map (getAlpha p) es
  getAlpha (x,y) (x0,y0) = (y0-y)/(x0-x)

-- given a list of range, test if intersections exist
hasIntersect :: [(Double,Double)] -> Bool
hasIntersect rs = maximum (map fst rs) <= minimum (map snd rs)

canApproximate :: Path -> Bool
canApproximate es = or results where
  squares = map (buildSquare . fst) es
  vs = head squares
  restSquares = tail squares
  results = map (\v -> hasIntersect $ map (rangeSquare v) restSquares) vs

isStraight :: Path -> (Int,Int) -> Bool
isStraight p (a,b) = (b-a) `mod` length p<=3 || notAll newp && notAll extendedp && canApproximate newp && canApproximate extendedp where
  notAll = (<4) . length . nub . map edgeDirection
  newp = cutPath p (a `mod` length p) (b `mod` length p)
  a' = (a-1) `mod` length p
  b' = (b+1) `mod` length p
  extendedp = cutPath p a' b'

nextCut :: Path -> [Int] -> [Int]
nextCut p cs = new:cs where
  new = snd $ last $ takeWhile (isStraight p) [(head cs, j) | j<-[head cs+1..last cs + length p - 1] ]

-- get a single polygon starting from specified position
polygonAt :: Path -> Int -> [Int]
polygonAt p x = maxPolygon p [x] where
  maxPolygon path ps = if isStraight path (head ps, last ps-1) then map (`mod` length p) ps else maxPolygon path (nextCut path ps)

allPolygon :: Path -> [[Int]]
allPolygon path = findAllP path ([], []) where
  findAllP pt (m,ps) = if length m >= length pt then ps else
    findAllP pt (m++newPath, newPath:ps) where
      newPath = polygonAt path $ head [ x | x<-[0..length path], x `notElem` m]

-- just pick the one path with fewest segments
optimalPolygon :: Path -> [(Word16,Word16)]
optimalPolygon path = map (fst . (path !!)) index where
  index = reverse . minimumBy (\x y -> compare (length x) (length y)) . allPolygon $ path
