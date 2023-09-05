-- | lua commands that are registered in Luau.hs are defined here.
module Luau.Command where
-- commands for lua are defined
import Prelude()
import UPrelude
import qualified HsLua as Lua
import Data.List.Split (splitOn)
import Data.Maybe ( fromMaybe )
import Numeric ( readHex )
import Data (ID(..), FPS(..))
import Util (newID)
import Text.Read ( readMaybe )
import Prog.Data
import Sign.Data
import Sign.Queue ( writeQueue, readChan, tryReadChan )
import Sign.Var ( atomically, readTVar )
import Sign.Util ( writeQueue'', writeChan'', readTVar'', clearTVar )
import Load.Data
import Luau.Util ( vtail, vhead, luaEvent )
import Luau.Data ( ShellCmd(..), UserVar(..) )
import Vulk.Font ( Font(..) )
import qualified Vulk.GLFW as GLFW

-- | quits everything using glfw
hsExit ∷ Env → Lua.Lua ()
hsExit env = luaEvent env $ EventSys SysExit

-- | logs at level n, 1 being -v, 3 being -vvv,
--   0 being no verbosity whatsoever
hsLogDebug ∷ Env → Int → String → Lua.Lua ()
hsLogDebug env n str = luaEvent env $ EventLog (LogDebug n) $ "[Luau] " ⧺ str

-- | logs info, should not be used in production code
hsLogInfo ∷ Env → String → Lua.Lua ()
hsLogInfo env str = luaEvent env $ EventLog LogInfo $ "[Luau] " ⧺ str

-- | logs a string and ends the entire process and children
hsLogError ∷ Env → String → Lua.Lua ()
hsLogError env str = luaEvent env $ EventLog LogError $ "[Luau] " ⧺ str

-- | reads the settings for the input keys
hsRegisterInputKeys ∷ Env → String → Lua.Lua ()
hsRegisterInputKeys env str = Lua.liftIO $ writeQueue'' env InputQueue
  $ QCInpCmd $ InpState (ISCRegisterKeys str)
-- | reads the settings for the input keys
hsRegisterTileMap ∷ Env → String → Lua.Lua ()
hsRegisterTileMap env str = Lua.liftIO $ writeQueue'' env LoadQueue
  $ QCLoadCmd $ LoadState (LSCRegisterTileMap str)
-- | reads the settings for the input keys
hsRegisterTextureMap ∷ Env → String → Lua.Lua ()
hsRegisterTextureMap env str = Lua.liftIO $ writeQueue'' env LoadQueue
  $ QCLoadCmd $ LoadState (LSCRegisterTextureMap str)

-- | creates a new window
hsNewWindow ∷ Env → String → Lua.Lua String
hsNewWindow env name = do
  clearID env
  Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadNew $ LCWindow $ ID name
  readID env

-- | clears the IDChan
clearID ∷ Env → Lua.Lua ()
clearID env = Lua.liftIO $ clearTVar env IDTVar

-- | clears the UDChan
clearUD ∷ Env → Lua.Lua ()
clearUD env = Lua.liftIO $ clearTVar env UDTVar

-- | reads an id from the load thread
readID ∷ Env → Lua.Lua String
readID env = do
  idin ← Lua.liftIO $ readTVar'' env IDTVar
  case idin of
    Nothing              → readID env
    Just (TVID (ID id0)) → return id0
    Just _               → return "ERR"

-- | reads an id from the load thread
readUD ∷ Env → Lua.Lua UserVar
readUD env = do
  udin ← Lua.liftIO $ readTVar'' env UDTVar
  case udin of
    Nothing              → readUD env
    Just (TVUD var)      → return var
    Just _               → return UVNULL

-- | selects a new window
hsSelectWin ∷ Env → String → Lua.Lua ()
hsSelectWin env name = Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadState $ LSCSelectWin $ ID name

-- | returns a random ID
hsNewID ∷ Env → Lua.Lua String
hsNewID env = do
  clearID env
  Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd LoadID
  readID env

-- | creates a new tile
hsNewTile ∷ Env → Double → Double → Double → Double → String → String → Lua.Lua String
hsNewTile env x y w h win t = do
  clearID env
  Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadNew $ LCTile (ID win) (TilePos (x,y) (w,h)) t
  readID env

-- | creates a new tile
hsNewAtlas ∷ Env → Double → Double → Double → Double → String
           → String → Int → Int → Lua.Lua String
hsNewAtlas env x y w h win t tx ty = do
  clearID env
  Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadNew $ LCAtlas (ID win) (TilePos (x,y) (w,h)) t (tx,ty)
  readID env

-- | create a new section of text
hsNewText ∷ Env → Double → Double → Double → Double
  → String → String → String → Lua.Lua String
hsNewText env x y w h win font text = do
  clearID env
  Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadNew
             $ LCText (ID win) $ Text IDNULL (x,y) (w,h) (ID font) text False
  readID env

-- | create a new button
hsNewLink ∷ Env → Double → Double → Double → Double
  → String → String → String → String → Lua.Lua String
hsNewLink env x y w h win font text link = do
  clearID env
  Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadNew
             $ LCButton (ID win) (Text IDNULL (x,y) (w,h) (ID font) text False) (BFLink (ID link))
  id0 ← readID env
  return id0

-- | starts the lua thread
hsStart ∷ Env → Lua.Lua ()
hsStart env = do
  luaEvent env $ EventLog (LogDebug 1) $ "[Luau] sending lua start command..."
  Lua.liftIO $ writeChan'' env LuaChan TStart

-- | reloads the command buffers of the engine
hsReload ∷ Env → Lua.Lua ()
hsReload env = Lua.liftIO $ writeQueue'' env LoadQueue
  $ QCLoadCmd LoadReload

-- | reloads the command buffers of the engine
hsRecreate ∷ Env → Lua.Lua ()
hsRecreate env = do
  luaEvent env $ EventLog (LogDebug 1) $ "[Luau] recreating..."
  Lua.liftIO $ writeQueue'' env LoadQueue
    $ QCLoadCmd LoadRecreate

-- | sends the font string to the main thread where we send
--   a font load cmd to the load thread
hsLoadFont ∷ Env → String → Lua.Lua String
hsLoadFont env fp = do
  ID id0 ← Lua.liftIO newID
  Lua.liftIO $ writeQueue'' env EventQueue
    $ QCEvent $ EventLoadFont $ Font fp (ID id0) Nothing (-1)
  return id0

-- | gets the current window size
hsGetWindowSize ∷ Env → Lua.Lua [Int]
hsGetWindowSize env = do
  clearUD env
  Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadGet GCWindow
  var ← readUD env
  case var of
    UVWindow win → do
      (x,y) ← Lua.liftIO $ GLFW.getWindowSize win
      return [x,y]
    _ → do
      luaEvent env $ EventLog LogError $ "[Luau] thats not a window size"
      return []

-- | adds a world object to a window
hsNewWorld ∷ Env → String → Lua.Lua ()
hsNewWorld env win = Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadNew
                                $ LCWorld (ID win)

-- | sets the debug level of the window
hsSetDebug ∷ Env → String → Lua.Lua ()
hsSetDebug env "fps"  = Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd
                                   $ LoadState $ LSCSetDebugLevel
                                   $ DebugFPS $ FPS 0 0 False
hsSetDebug env "null" = Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd
                                   $ LoadState $ LSCSetDebugLevel DebugNULL
hsSetDebug env level  = luaEvent env $ EventLog LogWarn
                       $ "[Luau] unknown debug level" ⧺ show level
