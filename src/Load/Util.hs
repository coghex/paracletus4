module Load.Util where

import Prelude ()
import UPrelude
import Load.Data ( Tile(..), TilePos(..), TileTex(..) )

emptyTiles ∷ Int → [Tile]
emptyTiles n = take n $ repeat $ Tile (TilePos (0,0) (1,1))
                                      (TileTex (0,0) (1,1) 0)
