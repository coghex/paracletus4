module Game.World where
import Prelude()
import UPrelude
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

generateWorldData ∷ [[WorldTile]]
generateWorldData = replicate h (replicate w wt)
  where wt = WorldTile WTPlains
        w  = 10
        h  = 10

generateWorldTiles ∷ Int → World → TextureMap → [Tile]
generateWorldTiles _      (World Nothing   _   ) _  = []
generateWorldTiles offset (World (Just wd) curs) tm = reverse
  $ generateWorldTilesFunc offset curs (0,0) wd tm
generateWorldTilesFunc ∷ Int → (Int,Int,Int) → (Double,Double)
  → [[WorldTile]] → TextureMap → [Tile]
generateWorldTilesFunc _      _    _   []       _  = []
generateWorldTilesFunc offset curs pos (wt:wts) tm
  = reverse (generateRowTiles offset curs pos wt tm)
  ⧺ generateWorldTilesFunc offset curs pos' wts tm
    where pos' = (fst pos + 1, snd pos - 0.5)
generateRowTiles ∷ Int → (Int,Int,Int) → (Double,Double)
  → [WorldTile] → TextureMap → [Tile]
generateRowTiles _      _    _   []       _  = []
generateRowTiles offset curs pos (wt:wts) tm
  = t : generateRowTiles offset curs pos' wts tm
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
