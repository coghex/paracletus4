-- | buffers contain abstract tiles, then provide functions
--   to turn them into dynamic data
module Prog.Buff where
-- a buffer of invisibly dynamic tiles to manipulate
import Prelude()
import UPrelude
import Data ( Color(..) )
import Data.Map (Map)
import qualified Data.Map as Map
import Load.Data ( Tile(..), DynData(..), TilePos(..), TileTex(..) )
import Vulk.Trans ( texDynDataFrame, dynDataFrame )

generateDynData ∷ Int → [Tile] → [DynData]
generateDynData _        [] = []
generateDynData fontsize ((Tile id (TilePos (x,y) (w,h)) (TileTex texi texs tex)):ts)
  = DynData dataF texDF : generateDynData fontsize ts
      where dataF = dynDataFrame pos scale
            texDF = texDynDataFrame (Color 255 255 255 255) texi $ tex + fontsize
            pos   = (realToFrac x,realToFrac y)
            scale = (realToFrac w,realToFrac h)
