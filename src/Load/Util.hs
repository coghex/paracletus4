module Load.Util where

import Prelude ()
import Data ( ID(..) )
import UPrelude
import Load.Data ( Tile(..), TilePos(..), TileTex(..) )

emptyTiles ∷ Int → [Tile]
emptyTiles n = take n $ repeat $ Tile IDNULL
                                      (TilePos (0,0) (4,4))
                                      (TileTex (0,0) (1,1) 0)
