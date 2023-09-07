module Game.World where
import Prelude()
import UPrelude
import Data.Bifunctor ( bimap )
import qualified Data.Map as Map
import Sign.Log
import Data ( ID(..) )
import Util ( blackColor )
import Load.Data
import Load.Util ( addElemToWin, findAtlas, findTex, emptyTile )
import Game.Data

generateWorld ∷ (MonadLog μ, MonadFail μ) ⇒
  ID → DrawState → LogT μ DrawState
generateWorld win ds = do
  return ds

newWorld ∷ ID → DrawState → DrawState
newWorld win ds = ds { dsWins = addElemToWin (dsWins ds) win (WinElemWorld world) }
  where world = World Nothing (0,0,0)

generateWorldData ∷ (Int,Int) → (Int,Int,Int) → [[WorldTile]]
generateWorldData (ww,wh) curs = createWorldData len len
  where w   = round $ realToFrac ww / 32.0
        h   = round $ realToFrac wh / 32.0
        len = max w h
createWorldData ∷ Int → Int → [[WorldTile]]
createWorldData x 0 = []
createWorldData x n = row : createWorldData x (n-1)
  where row = createWorldDataRow (n-1) x
createWorldDataRow ∷ Int → Int → [WorldTile]
createWorldDataRow y 0 = []
createWorldDataRow y n = wt : createWorldDataRow y (n-1)
  where wt = WorldTile WTPlains (n-1,y) False

generateWorldTiles ∷ Int → World → TextureMap → [Tile]
generateWorldTiles _      (World Nothing   _)          _  = []
generateWorldTiles offset (World (Just wd) (cx,cy,cz)) tm = selTile ⧺ (reverse
  $ generateWorldTilesFunc offset (0,0) wd tm)
    where selTile = generateSelectTile offset (World (Just wd) (cx,cy,cz)) tm
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
worldTileTex offset (WorldTile WTNULL pos sel)   tm'
  = findAtlas offset tind "nullTile" tm
  where tind          = (0,0)
        TextureMap tm = tm'
worldTileTex offset (WorldTile WTPlains pos sel) tm'
  = findAtlas offset tind "plainsTile" tm
  where tind          = (0,0)
        TextureMap tm = tm'

-- | generates tile data for the selected tiles
generateSelectTile ∷ Int → World → TextureMap → [Tile]
generateSelectTile offset (World (Just wd) (cx,cy,cz)) tm
  = generateSelectTileF offset wd tm
generateSelectTileF ∷ Int → [[WorldTile]] → TextureMap → [Tile]
generateSelectTileF offset []        tm = []
generateSelectTileF offset (row:wts) tm = row' ⧺ generateSelectTileF offset wts tm
  where row' = generateSelectTileRow offset row tm
generateSelectTileRow ∷ Int → [WorldTile] → TextureMap → [Tile]
generateSelectTileRow offset []       tm = []
generateSelectTileRow offset ((WorldTile typ pos sel):wts) (TextureMap tm)
  | sel       = Tile IDNULL (TilePos pos' (1,1))
                            (findTex offset "outlineTile" tm)
                            (TileBhv True)
                              : generateSelectTileRow offset wts (TextureMap tm)
  | otherwise = emptyTile offset
                              : generateSelectTileRow offset wts (TextureMap tm)
      where pos' = normalizeCart (bimap realToFrac realToFrac pos)

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

-- | normalizes coordinates into isometric form
normalizeIso ∷ (Double,Double) → (Double,Double)
normalizeIso (x,y) = (x-2*y,x+2*y)
-- | normalizes coordinates into cartesian form
normalizeCart ∷ (Double,Double) → (Double,Double)
normalizeCart (x,y) = (2*x',2*y')
  where x' = x/2+y/2
        y' = y/4-x/4

-- | processes a click on the world
clickWorld ∷ (Double,Double) → World → World
clickWorld pos (World dat curs) = World dat' curs
  where dat' = selectWorldTile pos dat
selectWorldTile ∷ (Double,Double) → Maybe [[WorldTile]] → Maybe [[WorldTile]]
selectWorldTile _   Nothing   = Nothing
selectWorldTile pos (Just wd) = Just $ selectWorldTileF pos wd
selectWorldTileF ∷ (Double,Double) → [[WorldTile]] → [[WorldTile]]
selectWorldTileF _   []       = []
selectWorldTileF pos (wt:wts) = wt' : selectWorldTileF pos wts
  where wt' = selectWorldTileRow pos wt
selectWorldTileRow ∷ (Double,Double) → [WorldTile] → [WorldTile]
selectWorldTileRow _   []       = []
selectWorldTileRow pos (wt:wts) = wt' : selectWorldTileRow pos wts
  where wt' = selectWorldTileSpot pos wt
selectWorldTileSpot ∷ (Double,Double) → WorldTile → WorldTile
selectWorldTileSpot pos (WorldTile typ tp _)
  | pos' ≡ tp = WorldTile typ tp True
  | otherwise = WorldTile typ tp False
      where pos' = bimap round round pos

-- | prints a world
printWorld ∷ Map.Map ID Window → String 
printWorld wins = foldl (⧺) "" (Map.map printWorldF wins)
printWorldF ∷ Window → String
printWorldF (Window _ elems) = printWorldInElems elems
printWorldInElems ∷ [WinElem] → String
printWorldInElems []       = []
printWorldInElems (we:wes) = showwe ⧺ printWorldInElems wes
  where showwe = printWorldElem we
printWorldElem ∷ WinElem → String
printWorldElem (WinElemWorld world) = show world
printWorldElem _                    = []
