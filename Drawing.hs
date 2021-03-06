module Drawing where
import MapType
import HistoryType
import ImageTracer
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Word
import Data.Monoid ((<>))
import Data.List (sort)
import qualified Graphics.Gloss.Interface.IO.Game as GS
import Graphics.Gloss.Data.Bitmap
import qualified Alphabet as AL
import qualified Triangulation as TRI
import ConvexHull

-- the map of paths, the range of bmp , the size of the screen and the viewing position and scaling factor (base 1)
-- bool is to control close / open of minimap
type WorldType = ((PolygonMap, RangeMap, LocalMap, ProvCountryMap, CountryMap), (Word16, Word16), (Float, Float), Vertice, Float, Bool)

rectIntersect :: (Ord a, Integral a) => ((a,a),(a,a)) -> ((a,a),(a,a)) -> Bool
rectIntersect ((a,b),(c,d)) ((a',b'),(c',d')) = not $ a'>c || c'<a || b'>d || d'<b

-- given a rectangle form range, return a list of provinces that need to be shown
inRangeProv :: RangeMap -> (Vertice, Vertice) -> [ProvID]
inRangeProv rmap r = [fst x | x <- Map.toList rmap, snd x `rectIntersect` r ]

-- transform list of nodes into another coordinate system given its zero point
-- intended to transform result coming from optimalPolygon in ImageTracer
transPath :: Float -> Float -> [(Float, Float)] -> [(Float,Float)]
transPath x y = map (\(a,b) -> (a - x, y - b))

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

-- minimap mouse vertice capture: given screen size, zoom and clicked position, return a corrected viewpoint
-- corrected viewpoint not finished
goFromMiniMap :: (Float, Float) -> (Word16, Word16) -> Float -> (Float, Float) -> (Word16, Word16)
goFromMiniMap (_,_) (bw, bh) _ (mx,my) = (round x, round y) where
  (x,y) = (fromIntegral bw/2 + mx*8, fromIntegral bh/2 - my*8)


handleEvent :: GS.Event -> WorldType -> IO WorldType
handleEvent (GS.EventKey (GS.SpecialKey GS.KeyUp) GS.Up _ _) (ms, bsize , scr, vp, zoom, False) = return (ms, bsize, scr,  moveView vp bsize scr zoom (100/zoom) Up, zoom, False)
handleEvent (GS.EventKey (GS.SpecialKey GS.KeyDown) GS.Up _ _) (ms, bsize , scr, vp, zoom, False) = return (ms, bsize, scr, moveView vp bsize scr zoom (100/zoom) Dn,  zoom, False)
handleEvent (GS.EventKey (GS.SpecialKey GS.KeyLeft) GS.Up _ _) (ms, bsize , scr, vp, zoom, False) = return (ms, bsize, scr, moveView vp bsize scr zoom (100/zoom) Lf, zoom, False)
handleEvent (GS.EventKey (GS.SpecialKey GS.KeyRight) GS.Up _ _) (ms, bsize , scr, vp, zoom, False) = return (ms, bsize, scr, moveView vp bsize scr zoom (100/zoom) Rg, zoom, False)
handleEvent (GS.EventKey (GS.Char 'g') GS.Up _ _) (ms,b,s,v,z,m) = return (ms,b,s,v,z,not m)
handleEvent (GS.EventKey (GS.Char 'h') GS.Up _ _) (ms,b,s,_,z,False) = return (ms,b,s,constantinople,z,False)
handleEvent (GS.EventKey (GS.Char '=') GS.Up _ _) (ms,b,s,v,z,mini) = return (ms,b,s,v,z*1.25,mini)
handleEvent (GS.EventKey (GS.Char '-') GS.Up _ _) (ms,b,s,v,z,mini) = return (ms,b,s,v,z*0.8,mini)
handleEvent (GS.EventKey (GS.MouseButton GS.LeftButton) GS.Up _ mp) (ms,b,s,_,z, True) = return (ms,b,s, goFromMiniMap s b z mp, z, False)
handleEvent _ world = return world

coloredPolygon :: ([Word8], GS.Path) -> GS.Picture
coloredPolygon (a,b) = GS.color (GS.makeColorI (fromIntegral (head a)) (fromIntegral (a !! 1)) (fromIntegral (last a)) 255) $ GS.polygon b

coloredShape :: [Word8] -> GS.Picture -> GS.Picture
coloredShape cs p = GS.color (GS.makeColorI (fromIntegral (head cs)) (fromIntegral (cs !! 1)) (fromIntegral (last cs)) 255) p

emptyCountry :: Country
emptyCountry = Country 0 "" [172,179,181]

--assuming sorted
combineTuple :: (Eq a, Eq b) => [(a,[b])] -> [(a,[b])]
combineTuple [] = []
combineTuple [x] = [x]
combineTuple (x:xs) = if fst x == fst (head xs) then combineTuple (both:tail xs) else x:combineTuple xs where
  both = (fst x, snd x ++ snd (head xs))
combineT :: (Ord a, Ord b) => [(a,b)] -> [(a,[b])]
combineT =  combineTuple . map (\(a,b) -> (a,[b])) . sort

-- render the picture (pmap is the optimalPolygon map)
renderWorld :: WorldType -> IO GS.Picture
renderWorld ((pmap, rmap, lcmap, pcmap, ctmap), _ , (sw, sh), (vx, vy), zoom, False) = do
  let pvs = inRangeProv rmap $ calcViewFrame sw sh (fromIntegral vx) (fromIntegral vy) zoom
      names = map (fromMaybe "undefined" . (`Map.lookup` lcmap)) pvs
      ctp = map (map (transPath (fromIntegral vx) (fromIntegral vy)) . fromMaybe [] . (`Map.lookup` pmap)) pvs
      drawcurve = mconcat . map GS.polygon . thickBezier (2/zoom) (4/zoom)
      drawprov = mconcat . map drawcurve
      allbzs = map (concatMap (init . drawBezier (4/zoom))) ctp
      allbzs' = zip pvs allbzs
      thickbzs = map (concatMap (thickBezier (2/zoom) 1.1)) ctp
      thickerbzs = map (\(c,s) ->  coloredShape c $ drawprov s)  (zip colors ctp)
      thickshape = mconcat $ map (map GS.polygon) thickbzs
      countries = map (\(p,pts) -> (fromMaybe emptyCountry $ Map.lookup (fromMaybe 0 (Map.lookup p pcmap)) ctmap, pts)) allbzs'
      ctpvs = combineT countries
      colors = map (getcolor . fst) countries
      colorpart = mconcat $ zipWith coloredShape colors (map (mconcat . map GS.polygon . TRI.triangulate) allbzs)
      namedraws = mconcat $ zipWith (AL.renderName zoom) allbzs names
      countrynames = map (\(c,ps) -> AL.renderName zoom (quickHull (concat ps)) (countryname c) ) ctpvs
  if zoom>4 then
    return $ GS.scale zoom zoom $  colorpart <> mconcat thickshape <> namedraws
  else
    return $ GS.scale zoom zoom $ mconcat thickerbzs <> colorpart <> mconcat countrynames
renderWorld (_, _, _, _, _, True) = loadBMP "resources/miniterrain.bmp"

stepWorld :: Float -> WorldType -> IO WorldType
stepWorld _ = return
