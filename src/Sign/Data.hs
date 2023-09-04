-- | various ADTs for handling events, input and load data
module Sign.Data where
-- data for the main event queue is defined
import Prelude()
import UPrelude
import Data ( PrintArg(..), KeyMap(..), KeyFunc(..), ID(..) )
import Load.Data
import Luau.Data ( ShellCmd(..) )
import Vulk.Data ( Verts(..) )
import Vulk.Font ( Font(..) )
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
           -- | texture fp list from the load thread
           | EventTextures ![String]
           -- | fonts are loaded differently every other texture
           | EventLoadFont !Font
           -- | changes to the settings
           | EventSettings !SettingsChange
           -- | requests data from the main thread
           | EventGet !GetCommand
           -- | lowest level actions go here
           | EventSys !SysAction
           | EventTest
           deriving (Show, Eq)

-- | possible commands load thread can handle
data LoadCmd = LoadNew LoadChunk | LoadShell ShellCmd | LoadTimer TimerName
             | LoadState LoadStateChange | LoadReload | LoadRecreate | LoadTest
             | LoadID | LoadGet GetCommand | LoadInput LoadInputCmd | LoadGen ID
             | LoadCmdNULL deriving (Show, Eq)
-- | possible input coming to the load thread from the input thread
data LoadInputCmd = LIButton Button | LIToggleButtons [Button] Bool
                  | LIClearButtons
                  | LINULL deriving (Show, Eq)
-- | possible new data that can be made in the load thread
data LoadChunk = LCWindow ID
               | LCText ID Text
               | LCTile ID TilePos String
               | LCAtlas ID TilePos String (Int,Int)
               | LCButton ID Text ButtonFunc
               | LCWorld ID
               | LCNULL deriving (Show, Eq)
-- | possible data user can request from the load thread
data GetCommand = GCWindow
                | GCNULL deriving (Show,Eq)
-- | possible events the input thread can handle
data InpCmd  = InpEvent InputEvent | InpState InputStateChange
             | InpCmdNULL  deriving (Show, Eq)

-- | possible changes to the load state
data LoadStateChange = LSCRegisterTileMap String | LSCRegisterTextureMap String
                     | LSCSelectWin ID | LSCSetGLFWWindow GLFW.Window
                     | LSCNULL deriving (Show, Eq)
-- | possible names of timers
data TimerName = ShellCursorTimer | NULLTimer deriving (Show, Eq)

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
data InputStateChange = ISCRegisterKeys String
                      | ISCCapture Capture
                      | ISCNewElem InputElem
                      | ISCNULL deriving (Show, Eq)

-- | input state is simply the state of the input thread
data InputState = InputState { keyMap     ∷ KeyMap
                             , keyCap     ∷ Capture
                             , inputElems ∷ [InputElem]
                             , mouseSt    ∷ MouseState } deriving (Show, Eq)

-- | various window elems have a corresponding input elem
data InputElem = IEButt Button | IENULL deriving (Show, Eq)
-- | button input information
data Button = Button { bFunc ∷ ButtonFunc
                     , bID   ∷ ID
                     , bPos  ∷ (Double,Double)
                     , bSize ∷ (Double,Double) } deriving (Show, Eq)
-- | the type of captured input
data Capture = CaptureShell | CaptureNULL deriving (Show, Eq)

-- | the mouse state, updated every tick of the input thread
data MouseState = MouseState { mouse1   ∷ Maybe (Double,Double)
                             , mouse2   ∷ Maybe (Double,Double)
                             , mouse3   ∷ Maybe (Double,Double)
                             , mousePos ∷ (Double,Double) } deriving (Show, Eq)

-- | commands for functionality at the lowest level
data SysAction = SysRecreate | SysReload
               | SysFullScreen
               | SysWindowed Int Int Int Int
               | SysExit | SysNULL deriving (Show, Eq)

-- | possible changes to make to the settings
data SettingsChange = SettingsChangeKeyMap KeyMap
                    | SettingsChangeSavename String
                    | SettingsChangeNULL deriving (Show, Eq)

