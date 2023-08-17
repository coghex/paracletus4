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
import Prog.Data ( Env(..), QueueName(..), QueueCmd(..) )
import Sign.Data
    ( Event(EventSys, EventLog), LogLevel(..), InputStateChange(..), LoadCmd(..), LoadChunk(..)
    , SysAction(SysReload, SysExit, SysRecreate), InpCmd(..), LoadStateChange(..) )
import Sign.Queue ( writeQueue, readChan )
import Sign.Var ( atomically, readTVar )
import Sign.Util ( writeQueue'' )
import Load.Data ( Tile(..), TilePos(..), TileTex(..) )
import Luau.Util ( vtail, vhead, luaEvent )

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
  Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadNew $ LCWindow name
  ID id0 ← Lua.liftIO $ atomically $ readChan (envIDChan env)
  return id0
-- | selects a new window
hsSelectWin ∷ Env → String → Lua.Lua ()
hsSelectWin env name = Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadState $ LSCSelectWin name

-- | creates a new tile
hsNewTile ∷ Env → Double → Double → Double → Double → String → String → Lua.Lua String
hsNewTile env x y w h win t = do
  Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadNew $ LCTile win (TilePos (x,y) (w,h)) t
  ID id0 ← Lua.liftIO $ atomically $ readChan (envIDChan env)
  return id0

-- | creates a new tile
hsNewAtlas ∷ Env → Double → Double → Double → Double → String
           → String → Int → Int → Lua.Lua String
hsNewAtlas env x y w h win t tx ty = do
  Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadNew $ LCAtlas win (TilePos (x,y) (w,h)) t (tx,ty)
  ID id0 ← Lua.liftIO $ atomically $ readChan (envIDChan env)
  return id0

-- | reloads the command buffers of the engine
hsReload ∷ Env → Lua.Lua ()
hsReload env = Lua.liftIO $ writeQueue'' env LoadQueue
  $ QCLoadCmd $ LoadReload

-- | reloads the command buffers of the engine
hsRecreate ∷ Env → Lua.Lua ()
hsRecreate env = Lua.liftIO $ writeQueue'' env LoadQueue
  $ QCLoadCmd $ LoadRecreate
