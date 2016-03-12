import qualified Graphics.Gloss.Interface.IO.Game as GS
import Drawing
import IOOper


main :: IO ()
main = do
  (plgmap, rmap, lcmap, pcmap, ctmap, (i,j)) <- initData
  GS.playIO
    (GS.InWindow "EUIV Map Editor" (1280,720) (0,0))
    GS.azure
    1
    ((plgmap, rmap, lcmap, pcmap, ctmap), (i,j), (1920,1080),constantinople, 4.5, False)
    renderWorld
    handleEvent
    stepWorld
