module ImageTracer where
import MapType
import Control.Monad.State
import Data.Word
import Data.List (nub, minimumBy)

-- construct the first edge of a province from the upper left corner
firstEdge :: PixelPos -> Edge
firstEdge p = (a, b) where
  vs = toVertice p
  a = head vs
  b = vs !! 2

firstAltEdge :: PixelPos -> Edge
firstAltEdge p = (a , b) where
  vs = toVertice p
  a = last vs
  b = vs !! 1

getNextEdge :: ShapeMap -> Word16 -> State Path ()
getNextEdge m p = do
  es <- get
  let e = last es
      newV = head [x | x<- edgeToEdge e, isEdge m x p]
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
buildSquare :: Vertice -> [(Float,Float)]
buildSquare (a,b) = [(fromIntegral a-0.5,fromIntegral b-0.5),(fromIntegral a+0.5,fromIntegral b-0.5),(fromIntegral a-0.5,fromIntegral b+0.5),(fromIntegral a+0.5,fromIntegral b-0.5)]

-- given a point of departure, calculate the range of inclination of line that can intersects a given square (expressed as a list of vertices
rangeSquare :: (Float,Float) -> [(Float,Float)] -> (Float,Float)
rangeSquare p es = (minimum rg, maximum rg) where
  rg = map (getAlpha p) es
  getAlpha (x,y) (x0,y0) = (y0-y)/(x0-x)

-- given a list of range, test if intersections exist
hasIntersect :: [(Float,Float)] -> Bool
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

-- optimal but too slow
allPolygon :: Path -> [[Int]]
allPolygon path = findAllP path ([], []) where
  findAllP pt (m,ps) = if length m >= length pt then ps else
    findAllP pt (nub (m++newPath), newPath:ps) where
      newPath = polygonAt path $ head [ x | x<-[0..length path], x `notElem` m]

-- not perfect but fast, just calculate the first 3 polygons (since shortest segment is 3)
somePolygon :: Path -> [[Int]]
somePolygon path = map (polygonAt path) [0..2]

-- just pick the one path with fewest segments
optimalPolygon :: Path -> [(Word16,Word16)]
optimalPolygon path = map (fst . (path !!)) index where
  index = reverse . minimumBy (\x y -> compare (length x) (length y)) . somePolygon $ path

turnFloat :: Vertice -> (Float,Float)
turnFloat (x,y) = (fromIntegral x, fromIntegral y)

-- get control points from a path
getBezierControl :: [Vertice] -> [[(Float,Float)]]
getBezierControl vts = beziers where
  vs = map turnFloat vts
  connect points = zip points (tail points ++ [head points])
  polygon = connect vs
  -- get a polygon that each vertice is the middle point of the edge of the original polygon
  middlePolygon = connect $ map (\((x,y),(x0,y0)) -> ((x+x0)/2, (y+y0)/2)) polygon
  -- calculate the line segment attached to each vertice to represent control points
  cartLen ((a,b),(c,d)) = sqrt $ (c-a)*(c-a) + (d-b)*(d-b)
  cartRatio (l1,l2) = cartLen l1 / (cartLen l2 + cartLen l1)
  ratios = map cartRatio $ connect polygon
  ratioToDiff r ((x,y),(x0,y0)) = ((r*(x-x0),r*(y-y0)),((1-r)*(x0-x),(1-r)*(y0-y)))
  verticeToGroup p@(x,y) ((dx,dy),(dx0,dy0)) = [(x+dx,y+dy),p,(x+dx0,y+dy0)]
  segmentVertice = zipWith verticeToGroup (tail vs ++ [head vs]) $ zipWith ratioToDiff ratios middlePolygon
  beziers = map (\(a,b) -> [a !! 1, last a, head b, b !! 1]) $ connect segmentVertice

-- given a quadratic bezier curve, draw the curve in appropriate segments
dToIntDivide :: Float -> Float -> Int
dToIntDivide a b = round $ a/b

arrOne :: [Float]
arrOne = map (1/) [1..100]

-- eliminate the warnings
three :: Int
three = 3
two :: Int
two = 2

drawBezier :: Float -> [(Float,Float)] -> [(Float, Float)]
drawBezier stepSize input = getpoint times where
    (x0,y0) = head input
    (x1,y1) = input !! 1
    (x2,y2) = input !! 2
    (x3,y3) = last input
    getpoint = map (\t->((1-t)^three*x0+3*(1-t)^two*t*x1+3*(1-t)*t^two*x2+t^three*x3,(1-t)^three*y0+3*(1-t)^two*t*y1+3*(1-t)*t^two*y2+t^three*y3))
    times = map ((*(arrOne !! (steps-1))) . fromIntegral) [0..steps-1] ++ [1]
    steps = dToIntDivide (sqrt((x1-x0)^two+(y1-y0)^two)+sqrt((x2-x1)^two+(y2-y1)^two)+sqrt((x3-x2)^two+(y3-y2)^two)) stepSize
