{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | data for the load thread, including queue commands
module Load.Data where
-- data for the loading thread is found
import Prelude()
import UPrelude
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric.DataFrame ( Mat44f )
import GHC.Generics

-- | transformation matricies that apply to a tile
data DynData = DynData { ddDataF ∷ Mat44f
                       , ddTexDF ∷ Mat44f
                       } deriving (Show, Eq)
-- | a tile in abstract form
data Tile = Tile { tilePos ∷ TilePos
                 , tileTex ∷ TileTex } deriving (Show, Eq)
data TilePos = TilePos { tPos   ∷ (Double,Double)
                       , tScale ∷ (Double,Double) } deriving (Show, Eq)
-- | texture indexes into a png atlas of size tSize,
--   tT is the number in which it was loaded
data TileTex = TileTex { tInd   ∷ (Int,Int)
                       , tSize  ∷ (Int,Int)
                       , tT     ∷ Int } deriving (Show, Eq, Ord)

-- | the state of the load thread
data DrawState = DrawState
  { dsStatus    ∷ DSStatus -- ^ return status for thread
  -- | each texture has a fp and size
  , dsTexMap    ∷ TextureMap
  -- | abstract tiles contain 
  , dsTiles     ∷ [Tile]
  -- | list of dyns in same order as tiles
  , dsDyns      ∷ [DynData] } deriving (Show, Eq)


-- | status of the loading thread, allowing
--   us to return results of deeply nested
--   pure functions
data DSStatus = DSSExit
              | DSSNULL deriving (Show, Eq)

-- | mapping from image files to texture information
newtype TextureMap = TextureMap (Map String Tex) deriving (Show,Eq,Ord)

data Tex = Tex { tfp   ∷ String
               , tsize ∷ (Int,Int) } deriving (Show, Eq, Ord)

data TextureData = TextureData { fp ∷ String
                               , w  ∷ Int
                               , h  ∷ Int
                               , t  ∷ Int } deriving (Generic, Show, Eq)

data InTexJson   = InTexJson   { textureData ∷ [TextureData] } deriving (Generic, Show)
instance FromJSON InTexJson where
  parseJSON = withObject "InTexJson" $ \v → InTexJson
        <$> v .: "textureData"
instance ToJSON   InTexJson where
  toJSON (InTexJson textureData) =
    object ["textureData" .= textureData]
  toEncoding (InTexJson textureData) =
    pairs ("textureData" .= textureData)

instance FromJSON TextureData where
  parseJSON = withObject "textureData" $ \v -> TextureData
        <$> v .: "fp"
        <*> v .: "w"
        <*> v .: "h"
        <*> v .: "t"
instance ToJSON   TextureData where
    toJSON (TextureData fp w h t) =
        object ["fp" .= fp, "w" .= w, "h" .= h, "t" .= t]
    toEncoding (TextureData fp w h t) =
        pairs ("fp" .= fp <> "w" .= w <> "h" .= h <> "t" .= t)
