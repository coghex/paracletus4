module Game.Data where
import Prelude()
import UPrelude

data World = World { worldData   ∷ Maybe [[WorldTile]]
                   , worldCursor ∷ (Int,Int,Int) } deriving (Show, Eq)
data WorldTile = WorldTile { wtType ∷ WorldTileType } deriving (Show, Eq)
data WorldTileType = WTPlains | WTNULL deriving (Show,Eq)
