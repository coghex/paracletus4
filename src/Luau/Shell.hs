-- | functions for the shell
module Luau.Shell where
import Prelude()
import UPrelude
import Data ( Shell(..), ID(..) )
import Load.Data ( Tile(..), TilePos(..), TileTex(..) )

toggleShell ∷ Shell → Shell
toggleShell shell = shell { shLoaded = not (shLoaded shell) }

shTiles ∷ Int → Shell → [Tile]
shTiles fontsize sh  = tiles
    where pos        = (-10,5)
          width      = 8
          height     = 2
          width'     = 2.0 * fromIntegral (width+1)
          height'    = 2.0 * fromIntegral (height+1)
          postl      = pos
          postr      = ((fst pos) + width', snd pos)
          posbl      = (fst pos, (snd pos) - height')
          posbr      = ((fst pos) + width',(snd pos) - height')
          blanktile  = Tile IDNULL (TilePos (0,0) (1,1))
                                   (TileTex (0,0) (1,1) fontsize)
          filltile   = Tile IDNULL (TilePos pos (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-9))
          righttile  = Tile IDNULL (TilePos postr (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-8))
          toptile    = Tile IDNULL (TilePos postl (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-7))
          toprtile   = Tile IDNULL (TilePos postr (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-6))
          topltile   = Tile IDNULL (TilePos postl (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-5))
          bottile    = Tile IDNULL (TilePos posbl (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-4))
          botrtile   = Tile IDNULL (TilePos posbr (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-3))
          botltile   = Tile IDNULL (TilePos posbl (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-2))
          lefttile   = Tile IDNULL (TilePos postl (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-1))
          toptiles   = tileHor width toptile
          bottiles   = tileHor width bottile
          lefttiles  = tileVer height lefttile
          righttiles = tileVer height righttile
          fill       = tileFill width height filltile
          tiles'     = topltile : toprtile : botltile : botrtile
                     : (toptiles ⧺ bottiles ⧺ lefttiles ⧺ righttiles ⧺ fill)
          tiles      = case shLoaded sh of
                         False → take (length tiles') $ repeat blanktile
                         True  → tiles'

tileHor ∷ Int → Tile → [Tile]
tileHor 0 tile = []
tileHor n (Tile id (TilePos (x,y) (w,h)) tex) = tile' : tileHor (n-1) tile'
  where tile' = Tile id (TilePos (x+2,y) (w,h)) tex
tileVer ∷ Int → Tile → [Tile]
tileVer 0 tile = []
tileVer n (Tile id (TilePos (x,y) (w,h)) tex) = tile' : tileVer (n-1) tile'
  where tile' = Tile id (TilePos (x,y-2) (w,h)) tex
tileFill ∷ Int → Int → Tile → [Tile]
tileFill 0 h _                                   = []
tileFill w h (Tile id (TilePos (x,y) sc) tex) = tiles ⧺ (tileFill (w-1) h tile')
  where tile' = Tile id (TilePos (x+2,y) sc) tex
        tiles = tileVer h tile'



