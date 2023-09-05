{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | data for the load thread, including queue commands
module Load.Data where
-- data for the loading thread is found
import Prelude()
import UPrelude
import Data ( ID, Shell, Color, FPS )
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric.DataFrame ( Mat44f )
import GHC.Generics
import qualified Vulk.GLFW as GLFW
import Game.Data ( World )

-- | transformation matricies that apply to a tile
data DynData = DynData { ddDataF ∷ Mat44f
                       , ddTexDF ∷ Mat44f
                       } deriving (Show, Eq)
-- | a tile in abstract form
data Tile = Tile { tileID  ∷ ID
                 , tilePos ∷ TilePos
                 , tileTex ∷ TileTex
                 , tileBhv ∷ TileBhv } deriving (Show, Eq)
data TilePos = TilePos { tPos   ∷ (Double,Double)
                       , tScale ∷ (Double,Double) } deriving (Show, Eq)
-- | texture indexes into a png atlas of size tSize,
--   tT is the number in which it was loaded
data TileTex = TileTex { tInd   ∷ (Int,Int)
                       , tSize  ∷ (Int,Int)
                       , tT     ∷ Int
                       , tColor ∷ Color } deriving (Show, Eq, Ord)
-- | some tiles cant be moved, so we keep track here
data TileBhv = TileBhv { tMoves ∷ Bool } deriving (Show, Eq)

-- | the state of the load thread
data DrawState = DrawState
  { dsStatus    ∷ DSStatus -- ^ return status for thread
  -- | a reference to the glfw window
  , dsWindow    ∷ Maybe GLFW.Window
  -- | each texture has a fp and size
  , dsTexMap    ∷ TextureMap
  -- | list of window objects
  , dsWins      ∷ Map.Map ID Window
  -- | current window
  , dsCurr      ∷ ID
  -- | the shell is completely seperate from all windowing
  , dsShell     ∷ Shell
  -- | a variable to decide if we show a loading screen
  , dsLoad      ∷ LoadState
  -- | we keep a copy of the camera in the load thread
  , dsCamera    ∷ (Double,Double,Double)
  -- | debug level shows various info on the screen
  , dsDebug     ∷ DebugLevel
  } deriving (Show, Eq)

-- | the state of the load thread meaning whether we have loaded or not
data LoadState = Loading | Loaded deriving (Show, Eq)

-- | level at which we show extra info on the screen
data DebugLevel = DebugFPS FPS | DebugNULL deriving (Show, Eq)

-- | windows contain elements that get converted to tiles and dyns
data Window = Window { winID    ∷ ID
                     , winElems ∷ [WinElem]
                     } deriving (Show, Eq)
-- | window elements define various things that can be interacted with
data WinElem = WinElemTile Tile
             | WinElemText Text
             | WinElemButton Text ID ButtonFunc ButtonState
             | WinElemWorld World
             | WinElemNULL deriving (Show, Eq)
-- | possible button states
data ButtonState = BSSelected | BSNULL deriving (Show, Eq)
-- | poossible button functions
data ButtonFunc = BFLink ID | BFNULL deriving (Show, Eq)
-- | defines a section of text
data Text = Text { textID     ∷ ID
                 , textPos    ∷ (Double,Double)
                 , textSize   ∷ (Double,Double)
                 , textFont   ∷ ID
                 , textString ∷ String
                 , textMoves  ∷ Bool } deriving (Show, Eq)

-- | status of the loading thread, allowing
--   us to return results of deeply nested
--   pure functions
data DSStatus = DSSExit
              | DSSReload
              | DSSRecreate
              | DSSNULL deriving (Show, Eq)

-- | mapping from image files to texture information
newtype TextureMap = TextureMap { textures ∷ [(String,Tex)] } deriving (Show,Eq,Ord)

data Tex = Tex { tfp   ∷ String
               , tI    ∷ Int
               , tsize ∷ (Int,Int) } deriving (Show, Eq, Ord)

data TextureData = TextureData { name ∷ String
                               , fp   ∷ String } deriving (Generic, Show, Eq)

data AtlasData = AtlasData { aname ∷ String
                           , afp   ∷ String
                           , w     ∷ Int
                           , h     ∷ Int } deriving (Generic, Show, Eq)

data InTexJson   = InTexJson   { textureData ∷ [TextureData], atlasData ∷ [AtlasData] } deriving (Generic, Show)
instance FromJSON InTexJson where
  parseJSON = withObject "InTexJson" $ \v → InTexJson
        <$> v .: "textureData"
        <*> v .: "atlasData"
instance ToJSON   InTexJson where
  toJSON (InTexJson textureData atlasData) =
    object ["textureData" .= textureData, "atlasData" .= atlasData]
  toEncoding (InTexJson textureData atlasData) =
    pairs ("textureData" .= textureData <> "atlasData" .= atlasData)

instance FromJSON AtlasData where
  parseJSON = withObject "atlasData" $ \v -> AtlasData
        <$> v .: "name"
        <*> v .: "fp"
        <*> v .: "w"
        <*> v .: "h"
instance ToJSON   AtlasData where
    toJSON (AtlasData name fp w h) =
        object ["name" .= name, "fp" .= fp, "w" .= w, "h" .= h]
    toEncoding (AtlasData name fp w h) =
        pairs ("name" .= name <> "fp" .= fp <> "w" .= w <> "h" .= h)
instance FromJSON TextureData where
  parseJSON = withObject "textureData" $ \v -> TextureData
        <$> v .: "name"
        <*> v .: "fp"
instance ToJSON   TextureData where
    toJSON (TextureData name fp) =
        object ["name" .= name, "fp" .= fp]
    toEncoding (TextureData name fp) =
        pairs ("name" .= name <> "fp" .= fp)
