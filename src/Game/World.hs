module Game.World where
import Prelude()
import UPrelude
import qualified Data.Map as Map
import Sign.Log
import Data ( ID(..) )
import Util ( blackColor )
import Load.Data
import Load.Util ( addElemToWin, findAtlas )
import Game.Data

generateWorld ∷ (MonadLog μ, MonadFail μ) ⇒
  ID → DrawState → LogT μ DrawState
generateWorld win ds = do
  return ds

newWorld ∷ ID → DrawState → DrawState
newWorld win ds = ds { dsWins = addElemToWin (dsWins ds) win (WinElemWorld world) }
  where world = World Nothing (0,0,0)

generateWorldData ∷ (Int,Int) → (Int,Int,Int) → [[WorldTile]]
generateWorldData (ww,wh) curs = replicate len (replicate len wt)
  where wt  = WorldTile WTPlains
        w   = round $ realToFrac ww / 32.0
        h   = round $ realToFrac wh / 32.0
        len = 2 -- max w h

generateWorldTiles ∷ Int → World → TextureMap → [Tile]
generateWorldTiles _      (World Nothing   _)          _  = []
generateWorldTiles offset (World (Just wd) (cx,cy,cz)) tm = reverse
  $ generateWorldTilesFunc offset (0,0) wd tm
generateWorldTilesFunc ∷ Int → (Double,Double)
  → [[WorldTile]] → TextureMap → [Tile]
generateWorldTilesFunc _      _   []       _  = []
generateWorldTilesFunc offset pos (wt:wts) tm
  = reverse (generateRowTiles offset pos wt tm)
  ⧺ generateWorldTilesFunc offset pos' wts tm
    where pos'      = (fst pos + 1, snd pos - 0.5)
generateRowTiles ∷ Int → (Double,Double)
  → [WorldTile] → TextureMap → [Tile] 
generateRowTiles _      _   []       _  = []
generateRowTiles offset pos (wt:wts) tm
  = t : generateRowTiles offset pos' wts tm
  where t    = Tile IDNULL (TilePos pos (1,1))
                           (worldTileTex offset wt tm)
                           (TileBhv True)
        pos' = (fst pos + 1, snd pos + 0.5)
worldTileTex ∷ Int → WorldTile → TextureMap → TileTex
worldTileTex offset (WorldTile WTNULL)   tm'
  = findAtlas offset tind "nullTile" tm
  where tind          = (0,0)
        TextureMap tm = tm'
worldTileTex offset (WorldTile WTPlains) tm'
  = findAtlas offset tind "plainsTile" tm
  where tind          = (0,0)
        TextureMap tm = tm'

-- | moves the cursor in the first world found
moveWorldCursor ∷ Map.Map ID Window → (Double,Double,Double) → Map.Map ID Window
moveWorldCursor wins cam = Map.map (moveWorldCursorInWins cam) wins
moveWorldCursorInWins ∷ (Double,Double,Double) → Window → Window
moveWorldCursorInWins cam win
  = win { winElems = moveWorldCursorInElems cam (winElems win) }
moveWorldCursorInElems ∷ (Double,Double,Double) → [WinElem] → [WinElem]
moveWorldCursorInElems _          []                                   = []
moveWorldCursorInElems (cx,cy,cz) ((WinElemWorld (World dat curs)):ws)
  = w' : moveWorldCursorInElems (cx,cy,cz) ws
  where w'    = WinElemWorld $ World dat curs'
        curs' = (round (cx/64.0), round (cy/64.0), round cz)
moveWorldCursorInElems cam        (w:ws)
  = w : moveWorldCursorInElems cam ws
