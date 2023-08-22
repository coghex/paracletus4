-- | functions for the shell
module Luau.Shell where
import Prelude()
import UPrelude
import Data ( Shell(..), ID(..) )
import Load.Data ( Tile(..), TilePos(..), TileTex(..) )

toggleShell ∷ Shell → Shell
toggleShell shell = shell { shLoaded = not (shLoaded shell) }

shTiles ∷ Int → Shell → [Tile]
shTiles fontsize sh = case shLoaded sh of
  False → [Tile IDNULL (TilePos (-5,3) (1,1)) (TileTex (0,0) (1,1) fontsize)]
  True  → topltile
    where filltile = [Tile IDNULL (TilePos (-5,3) (1,1))
                                  (TileTex (0,0) (1,1) (fontsize-9))]
          lefttile = [Tile IDNULL (TilePos (-5,3) (1,1))
                                  (TileTex (0,0) (1,1) (fontsize-8))]
          toptile  = [Tile IDNULL (TilePos (-5,3) (1,1))
                                  (TileTex (0,0) (1,1) (fontsize-7))]
          toprtile = [Tile IDNULL (TilePos (-5,3) (1,1))
                                  (TileTex (0,0) (1,1) (fontsize-6))]
          topltile = [Tile IDNULL (TilePos (-5,3) (1,1))
                                  (TileTex (0,0) (1,1) (fontsize-5))]
