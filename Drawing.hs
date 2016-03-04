module Drawing where
import MapType
import ImageTracer
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Word
import qualified Graphics.Gloss.Interface.IO.Game as GS

-- the map of paths, the range of bmp , the size of the screen and the viewing position and scaling factor (base 1)
type WorldType = (Map.Map Word16 [Vertice], RangeMap, (Word16, Word16), (Float, Float), Vertice, Float)

rectIntersect :: (Ord a, Integral a) => ((a,a),(a,a)) -> ((a,a),(a,a)) -> Bool
rectIntersect ((a,b),(c,d)) ((a',b'),(c',d')) = not $ a'>c || c'<a || b'>d || d'<b

-- given a rectangle form range, return a list of provinces that need to be shown
inRangeProv :: RangeMap -> (Vertice, Vertice) -> [ProvID]
inRangeProv rmap r = [fst x | x <- Map.toList rmap, snd x `rectIntersect` r ]

-- transform list of nodes into another coordinate system given its zero point
-- intended to transform result coming from optimalPolygon in ImageTracer
transPath :: Int -> Int -> [Vertice] -> [(Int,Int)]
transPath x y = map (\(a,b) -> (fromIntegral a - x, fromIntegral b - y))

-- perhaps a good starting position
constantinople :: Vertice
constantinople = (3260,700)

-- given visual center and scale, calculate the range of pixels viewable on original bmp
calcViewFrame :: Float -> Float -> Float -> Float -> Float -> (Vertice, Vertice)
calcViewFrame flwidth flheight x y sz = ((round ul, round ur), (round dl, round dr)) where
  (ul, ur) = (x - flwidth / 2 / sz , y - flheight / 2 / sz)
  (dl, dr) = (x + flwidth / 2 / sz , y + flheight / 2 / sz)

-- given current viewpoint, bmp size, screen size, zooming factor, steps to move and direction, move the viewpoint
moveView :: Vertice -> (Word16 , Word16) -> (Float, Float) ->  Float -> Float -> Direction -> Vertice
moveView v (bw, bh) (sw, sh) zoom len drn = (round x0, round y0) where
  (szW, szH) = (sw / zoom, sh / zoom )
  (bzW, bzH) = (fromIntegral bw, fromIntegral bh)
  (minX, minY) = (3+szW, 3+szH)
  (maxX, maxY) = (bzW - szW - 2, bzH - szH - 2)
  directTurn (x,y) l Up = (fromIntegral x, fromIntegral y - l)
  directTurn (x,y) l Dn = (fromIntegral x, fromIntegral y + l)
  directTurn (x,y) l Lf = (fromIntegral x - l, fromIntegral y)
  directTurn (x,y) l Rg = (fromIntegral x + l, fromIntegral y)
  (vx0, vy0) = directTurn v len drn
  (x0,y0) = (min (max minX vx0) maxX, min (max minY vy0) maxY)

handleEvent :: GS.Event -> WorldType -> IO WorldType
handleEvent (GS.EventKey (GS.SpecialKey GS.KeyUp) GS.Down _ _) (pmap, rmap, bsize , scr, vp, zoom) = return (pmap , rmap, moveView vp bsize scr zoom (2/zoom) Up, scr, vp, zoom)
handleEvent (GS.EventKey (GS.SpecialKey GS.KeyDown) GS.Down _ _) (pmap, rmap, bsize , scr, vp, zoom) = return (pmap , rmap, moveView vp bsize scr zoom (2/zoom) Dn, scr, vp, zoom)
handleEvent (GS.EventKey (GS.SpecialKey GS.KeyLeft) GS.Down _ _) (pmap, rmap, bsize , scr, vp, zoom) = return (pmap , rmap, moveView vp bsize scr zoom (2/zoom) Lf, scr, vp, zoom)
handleEvent (GS.EventKey (GS.SpecialKey GS.KeyRight) GS.Down _ _) (pmap, rmap, bsize , scr, vp, zoom) = return (pmap , rmap, moveView vp bsize scr zoom (2/zoom) Rg, scr, vp, zoom)
handleEvent _ world = return world

-- render the picture (pmap is the optimalPolygon map)
renderWorld :: WorldType -> IO GS.Picture
renderWorld (pmap, rmap, _ , (sw, sh), (vx, vy), zoom) = do
  let pvs = inRangeProv rmap $ calcViewFrame sw sh (fromIntegral vx) (fromIntegral vy) zoom
      allbzs = map (concatMap (drawBezier (1/zoom)). getBezierControl . transPath (fromIntegral vx) (fromIntegral vy) . fromMaybe [] . (`Map.lookup` pmap)) pvs
  return $ mconcat $ map GS.line allbzs

stepWorld :: Float -> WorldType -> IO WorldType
stepWorld _ = return
