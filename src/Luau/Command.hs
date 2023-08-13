-- | lua commands that are registered in Luau.hs are defined here.
module Luau.Command where
-- commands for lua are defined
import Prelude()
import UPrelude
import qualified HsLua as Lua
import Data.List.Split (splitOn)
import Data.Maybe ( fromMaybe )
import Numeric ( readHex )
import Text.Read ( readMaybe )
import Prog.Data ( Env(..) )
import Sign.Data
    ( Event(EventSys, EventLog), LogLevel(..),
      SysAction(SysReload, SysExit, SysRecreate) )
import Sign.Queue ( writeQueue )
import Sign.Var ( atomically, readTVar )
import Luau.Data ( Window(..), Page(..) )
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
