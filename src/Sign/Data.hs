-- | various ADTs for handling events, input and load data
module Sign.Data where
-- data for the main event queue is defined
import Prelude()
import UPrelude
import Data ( PrintArg(..), KeyMap(..), KeyFunc(..) )
import Load.Data ( Tile, DrawState, TilePos, DynData(..), TextureMap(..), Text(..) )
import Vulk.Data ( Verts(..) )
import qualified Data.Map as Map
import qualified Vulk.GLFW as GLFW

-- | timer state is used for all the timers
data TState = TStart | TStop | TPause | TNULL deriving (Show, Eq)

-- | return state for a threaded command event
data EventResult = EventResultSuccess
                 | EventResultInputState InputState
                 | EventResultError String deriving (Show, Eq)
-- | load command results
data LoadResult = LoadResultSuccess
                | LoadResultDrawState DrawState
                | LoadResultError String deriving (Show, Eq)

-- | events processed by the main thread
data Event = EventError !GLFW.Error !String -- GLFW specific
           -- | logs into the monadic logger, but also allows stdout
           | EventLog !LogLevel !String
           -- | key/mouse input, mostly from GLFW callbacks
           | EventInput !InputEvent
           -- | verticies, indicies, and dynamic data from the load thread
           | EventLoad !LoadData
           -- | texture fp list from the load thread
           | EventTextures ![String]
           -- | fonts are loaded differently every other texture
           | EventLoadFont !String
           -- | changes to the settings
           | EventSettings !SettingsChange
           -- | lowest level actions go here
           | EventSys !SysAction
           deriving (Show, Eq)

-- | possible commands load thread can handle
data LoadCmd = LoadNew LoadChunk
             | LoadState LoadStateChange | LoadReload | LoadRecreate | LoadTest
             | LoadCmdNULL deriving (Show, Eq)
data LoadChunk = LCWindow String
               | LCText String Text
               | LCTile String TilePos String
               | LCAtlas String TilePos String (Int,Int)
               | LCNULL deriving (Show, Eq)
-- | possible events the input thread can handle
data InpCmd  = InpEvent InputEvent | InpState InputStateChange
             | InpCmdNULL  deriving (Show, Eq)

-- | possible changes to the load state
data LoadStateChange = LSCRegisterTileMap String | LSCRegisterTextureMap String
                     | LSCSelectWin String
                     | LSCNULL deriving (Show, Eq)

-- | log levels are for monadic logger, but stdio
data LogLevel = LogDebug Int
              | LogInfo
              | LogWarn
              | LogPrint PrintArg
              | LogError
              | LogNULL deriving (Show, Eq)

-- | input sources enumerated
data InputEvent
  = InputKey !GLFW.Window !GLFW.Key !Int !GLFW.KeyState
      !GLFW.ModifierKeys
  | InputMouseButton !GLFW.Window !GLFW.MouseButton
      !GLFW.MouseButtonState !GLFW.ModifierKeys
  | InputMouseScroll !GLFW.Window !Double !Double
  deriving (Show, Eq)

-- | possible changes to the input state
data InputStateChange = ISCRegisterKeys String | ISCNULL deriving (Show, Eq)

-- | input state is simply the state of the input thread
data InputState = InputState { keyMap ∷ KeyMap } deriving (Show, Eq)

-- | data gets loaded in from a seperate thread
data LoadData = LoadData !Verts ![DynData] deriving (Show,Eq)

-- | commands for functionality at the lowest level
data SysAction = SysRecreate | SysReload
               | SysFullScreen
               | SysWindowed Int Int Int Int
               | SysExit | SysNULL deriving (Show, Eq)

-- | possible changes to make to the settings
data SettingsChange = SettingsChangeKeyMap KeyMap
                    | SettingsChangeSavename String
                    | SettingsChangeNULL deriving (Show, Eq)

