-- | data for the continuation monad
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-} 
module Prog.Data where
-- the state is
import Prelude()
import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import Load.Data ( DynData )
import Sign.Data ( TState, Event(..), LoadCmd(..), InpCmd(..) )
import Sign.Except ( ProgExcept )
import Sign.Queue ( Queue, TChan )
import Sign.Var ( TVar )
import Time.Data ( TimeCmd )
import Data ( FPS, ID )
import Data.Time.Clock.System ( SystemTime )
import Data.Map (Map)
import Vulk.Data ( Verts )
import Vulk.Font ( TTFData )
import qualified HsLua as Lua
import qualified Vulk.GLFW as GLFW

-- | specific utility actions
data ProgResult = ProgSuccess | ProgError deriving (Show, Eq)
-- | glfw loop status
data LoopControl = ContinueLoop | AbortLoop deriving Eq
-- | a reload means reloading textures, objects, and command buffers
-- | a recreate means recreating the whole swapchain, implying reload
data ReloadState = RSReload | RSRecreate | RSNULL deriving (Show, Eq)

-- | env should only hold pointers/references, perfect for
-- | transactional memory and inter thread communication
data Env = Env { envQueues ∷ Queues
               , envChans  ∷ Chans
               , envTVars  ∷ TVars
               , envLuaSt  ∷ Lua.State }

-- | dynamic collection of queues
data Queues    = Queues { qm ∷ Map QueueName (Queue QueueCmd) }
data QueueCmd  = QCEvent Event | QCLoadCmd LoadCmd | QCTimeCmd TimeCmd
               | QCInpCmd InpCmd deriving (Show, Eq)
-- | some queues are required, others can be added
data QueueName = EventQueue | LoadQueue | TimeQueue
               | InputQueue | CustomQueue Int deriving (Show, Eq, Ord)
-- | dynamic collection of chans
data Chans     = Chans { cm ∷ Map ChanName (TChan TState) }
data ChanName  = LuaChan | InputChan | LoadChan | TimeChan | IDChan
               | CustomChan Int deriving (Show, Eq, Ord)
-- | dynamic collection of tvars
data TVars     = TVars { tm ∷ Map TVarName (TVar (Maybe TVarValue)) }
data TVarValue = TVInt Int | TVString String | TVVerts Verts | TVDyns [DynData]
               | TVFontMap [TTFData] | TVID ID deriving (Show, Eq)
data TVarName  = WindowTVar | VertsTVar | DynsTVar
               | FontSizeTVar | FontMapTVar | IDTVar
               | CustomTVar Int deriving (Show, Eq, Ord)

-- | state holds mutable data, and the
--   current status of the whole App
--   not much here, but its mission critical
data State = State { stStatus   ∷ ProgExcept
                   -- logging monadic function
                   , stLogFunc  ∷ Logger.Loc → Logger.LogSource
                                    → Logger.LogLevel → Logger.LogStr
                                    → IO ()
                   -- | the main glfw object
                   , stWindow   ∷ !(Maybe GLFW.Window)
                   -- | set this variable and the swapchain will reload
                   --   on the next loop iteration
                   , stReload   ∷ !ReloadState
                   -- | determines if we have loaded
                   , stLoaded   ∷ !Bool
                   -- | state of user settings
                   , stSettings ∷ !Settings
                   -- | the list of textures by fp loaded from outside
                   , stTextures ∷ ![String]
                   -- | the optional font must be loaded seperately
                   , stFont     ∷ !(Maybe String)
                   -- | variables for FPS calculation
                   , stStartT   ∷ !SystemTime
                   , stFPS      ∷ !FPS
                   , stTick     ∷ !(Maybe Double)
                   }

-- | defines some user alterable settings
--   needed too often to save on disk
data Settings = Settings { sFPSCap ∷ Maybe Int }
