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
  where world = World Nothing

generateWorldData ∷ (Int,Int) → [[WorldTile]]
generateWorldData (w,h) = replicate h (replicate w wt)
  where wt = WorldTile WTPlains

generateWorldTiles ∷ Int → World → TextureMap → [Tile]
generateWorldTiles _      (World Nothing)   _  = []
generateWorldTiles offset (World (Just wd)) tm = generateWorldTilesFunc offset (0,0) wd tm
generateWorldTilesFunc ∷ Int → (Double,Double) → [[WorldTile]] → TextureMap → [Tile]
generateWorldTilesFunc _      _   []       _  = []
generateWorldTilesFunc offset pos (wt:wts) tm = generateRowTiles offset pos wt tm
                                           ⧺ generateWorldTilesFunc offset pos' wts tm
  where pos' = (fst pos + 1, snd pos)
generateRowTiles ∷ Int → (Double,Double) → [WorldTile] → TextureMap → [Tile]
generateRowTiles _      _   []       _  = []
generateRowTiles offset pos (wt:wts) tm = t : generateRowTiles offset pos' wts tm
  where t    = Tile IDNULL (TilePos pos (1,1)) (worldTileTex offset wt tm)
        pos' = (fst pos, snd pos + 1)
worldTileTex ∷ Int → WorldTile → TextureMap → TileTex
worldTileTex offset (WorldTile WTNULL)   _   = TileTex (0,0) (1,1) offset blackColor
worldTileTex offset (WorldTile WTPlains) tm' = findAtlas offset tind "plainsTile" tm
  where tind          = (0,0)
        TextureMap tm = tm'
