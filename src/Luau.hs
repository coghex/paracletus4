-- | lua interpreter runs initLuau function from
--   each mod once at the beginning the runLuau
--   once for each mod every lua thread tick.
module Luau where
-- lua loader and interpreter
import Prelude()
import UPrelude
import Data.List (sort)
import Data.Maybe ( fromMaybe )
import Data.String ( fromString )
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified HsLua as Lua
import System.Directory (getDirectoryContents)
import System.FilePath (combine)
import Luau.Command
import Prog.Data ( Env(..) )
import Sign.Data
    ( Event(EventLog, EventSys),
      LogLevel(LogDebug),
      SysAction(SysRecreate),
      TState(..) )
import Sign.Thread (threadDelay)
import Sign.Queue (readChan, tryReadChan, writeQueue)
import Sign.Var (atomically)
import Sign.Util ( log' )

-- | initialization of each mod file, as well as registering all
--   of the raw functions, and kickoff of the vertex generation
luauThread ∷ Env → IO ()
luauThread env = do
   -- atomically $ writeQueue (findQueue EventQueue env)
   --   $ EventLog (LogDebug 1) "loading mod files:"
    log' env (LogDebug 1) "loading mod files:"
    return ()
