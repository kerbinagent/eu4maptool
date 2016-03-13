import Beijing
import Drawing
import ImageTracer
import Alphabet as ALP
import Triangulation as TRI
import qualified Graphics.Gloss.Interface.IO.Game as GS
import Data.Monoid ((<>))

renderBJ :: (Float,Float,Float) -> IO GS.Picture
renderBJ (x,y, zoom) = return $ GS.scale zoom zoom result where
  ctp =  getBezierControl $ optimalPolygon beijing
  (a,b,c) = getOutline curve
  centerr = getCenterRad a b c
  tctp = map (transPath x y) ctp
  curve = (concatMap (init . drawBezier (4/zoom))) tctp
  colorpart = (mconcat . map GS.polygon . TRI.triangulate) curve
  typepart = renderName (0.05) curve "Beijing"
  roundd = uncurry GS.translate (fst centerr) (GS.scale 0.1 0.1 $ GS.text ".")
  result = roundd <> colorpart <> (GS.color GS.red typepart) <> (GS.color GS.green ((uncurry GS.translate a (GS.scale 0.05 0.05 $ GS.text "1")) <> uncurry GS.translate b (GS.scale 0.05 0.05 $ GS.text "2") <> uncurry GS.translate c (GS.scale 0.05 0.05 $ GS.text "3")))

stepW _ = return

handle (GS.EventKey (GS.SpecialKey GS.KeyUp) GS.Up _ _) (x,y,z) = return (x,y-10,z)
handle (GS.EventKey (GS.SpecialKey GS.KeyDown) GS.Up _ _) (x,y,z) = return (x,y+10,z)
handle (GS.EventKey (GS.SpecialKey GS.KeyLeft) GS.Up _ _) (x,y,z) = return (x-10,y,z)
handle (GS.EventKey (GS.SpecialKey GS.KeyRight) GS.Up _ _) (x,y,z) = return (x+10,y,z)
handle (GS.EventKey (GS.SpecialKey GS.KeyPageUp) GS.Up _ _) (x,y,z) = return (x,y-10,z*1.2)
handle (GS.EventKey (GS.SpecialKey GS.KeyPageDown) GS.Up _ _) (x,y,z) = return (x,y-10,z*0.8)
handle _ b = return b


main :: IO ()
main = do
  GS.playIO
    (GS.InWindow "EUIV Map Editor" (1280,720) (0,0))
    GS.azure
    1
    (4640,723,4)
    renderBJ
    handle
    stepW
