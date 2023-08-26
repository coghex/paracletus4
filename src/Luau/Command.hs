-- | lua commands that are registered in Luau.hs are defined here.
module Luau.Command where
-- commands for lua are defined
import Prelude()
import UPrelude
import qualified HsLua as Lua
import Data.List.Split (splitOn)
import Data.Maybe ( fromMaybe )
import Numeric ( readHex )
import Data (ID(..))
import Text.Read ( readMaybe )
import Prog.Data
import Sign.Data
import Sign.Queue ( writeQueue, readChan, tryReadChan )
import Sign.Var ( atomically, readTVar )
import Sign.Util ( writeQueue'', writeChan'', readTVar'', clearTVar )
import Load.Data ( Tile(..), TilePos(..), TileTex(..), Text(..) )
import Luau.Util ( vtail, vhead, luaEvent )
import Luau.Data ( ShellCmd(..) )

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
  Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadNew $ LCWindow name
  readID env

-- | clears the IDChan
clearID ∷ Env → Lua.Lua ()
clearID env = Lua.liftIO $ clearTVar env IDTVar

-- | reads an id from the load thread
readID ∷ Env → Lua.Lua String
readID env = do
  idin ← Lua.liftIO $ readTVar'' env IDTVar
  case idin of
    Nothing              → readID env
    Just (TVID (ID id0)) → return id0
    Just _               → return "ERR"

-- | selects a new window
hsSelectWin ∷ Env → String → Lua.Lua ()
hsSelectWin env name = Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadState $ LSCSelectWin name

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
  Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadNew $ LCTile win (TilePos (x,y) (w,h)) t
  readID env

-- | creates a new tile
hsNewAtlas ∷ Env → Double → Double → Double → Double → String
           → String → Int → Int → Lua.Lua String
hsNewAtlas env x y w h win t tx ty = do
  clearID env
  Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadNew $ LCAtlas win (TilePos (x,y) (w,h)) t (tx,ty)
  readID env

-- | create a new section of text
hsNewText ∷ Env → Double → Double → Double → Double → String → String → Lua.Lua String
hsNewText env x y w h win text = do
  clearID env
  Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadNew
             $ LCText win $ Text IDNULL (x,y) (w,h) text
  readID env

-- | starts the lua thread
hsStart ∷ Env → Lua.Lua ()
hsStart env = do
  luaEvent env $ EventLog LogInfo $ "[Luau] sending lua start command..."
  Lua.liftIO $ writeChan'' env LuaChan TStart

-- | reloads the command buffers of the engine
hsReload ∷ Env → Lua.Lua ()
hsReload env = Lua.liftIO $ writeQueue'' env LoadQueue
  $ QCLoadCmd LoadReload

-- | reloads the command buffers of the engine
hsRecreate ∷ Env → Lua.Lua ()
hsRecreate env = do
  luaEvent env $ EventLog LogInfo $ "[Luau] recreating..."
  Lua.liftIO $ writeQueue'' env LoadQueue
    $ QCLoadCmd LoadRecreate

-- | sends the font string to the main thread where we send
--   a font load cmd to the load thread
hsLoadFont ∷ Env → String → Lua.Lua ()
hsLoadFont env fp = Lua.liftIO $ writeQueue'' env EventQueue
  $ QCEvent $ EventLoadFont fp
