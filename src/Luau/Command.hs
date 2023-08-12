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
import Luau.Util ( vtail, vhead )

-- | quits everything using glfw
hsExit ∷ Env → Lua.Lua ()
hsExit env = return () -- Lua.liftIO $ atomically
 -- $ writeQueue (envEventQ env) $ EventSys SysExit

