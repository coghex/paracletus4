-- | buffers contain abstract tiles, then provide functions
--   to turn them into dynamic data
module Prog.Buff where
-- a buffer of invisibly dynamic tiles to manipulate
import Prelude()
import UPrelude
import Data ( Color(..) )
import Data.Map (Map)
import qualified Data.Map as Map
import Load.Data ( Tile(..), DynData(..), Dyns(..) )
import Vulk.Trans ( texDynDataFrame, dynDataFrame )

generateDynData ∷ [Tile] → Dyns
generateDynData tiles = Dyns $ generateDynData' tiles
generateDynData' ∷ [Tile] → [DynData]
generateDynData' [] = []
generateDynData' ((Tile (x,y) (w,h) texi texs tex):ts)
  = DynData dataF texDF : generateDynData' ts
      where dataF = dynDataFrame pos scale
            texDF = texDynDataFrame (Color 255 255 255 255) texi tex
            pos   = (realToFrac x,realToFrac y)
            scale = (realToFrac w,realToFrac h)
