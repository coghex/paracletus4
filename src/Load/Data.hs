
-- | data for the load thread, including queue commands
module Load.Data where
-- data for the loading thread is found
import Prelude()
import UPrelude
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric.DataFrame ( Mat44f )

data DynData = DynData { ddDataF ∷ Mat44f
                       , ddTexDF ∷ Mat44f
                       } deriving (Show, Eq)
newtype Dyns = Dyns [DynData] deriving (Show, Eq)
data Tile = Tile { tPos   ∷ (Double,Double)
                 , tScale ∷ (Double,Double)
                 , tInd   ∷ (Int,Int)
                 , tSize  ∷ (Int,Int)
                 , tT     ∷ Int
                 } deriving (Show, Eq)
