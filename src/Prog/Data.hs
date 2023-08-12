-- | data for the continuation monad
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-} 
module Prog.Data where
-- the state is
import Prelude()
import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import Sign.Data ( TState )
import Sign.Except ( ProgExcept )
import Sign.Queue ( Queue, TChan )
import Sign.Var ( TVar )
import Data.Time.Clock.System ( SystemTime )
import Data.Map (Map)
import qualified HsLua as Lua
import qualified Vulk.GLFW as GLFW

-- | specific utility actions
data ProgResult = ProgSuccess | ProgError deriving (Show, Eq)
-- | glfw loop status
data LoopControl = ContinueLoop | AbortLoop deriving Eq

-- | env should only hold pointers/references, perfect for
-- | transactional memory and inter thread communication
data Env = Env { envQueues ∷ Queues
               , envChans  ∷ Chans
               , envTVars  ∷ TVars
               , envLuaSt  ∷ Lua.State }

-- | dynamic collection of queues
data Queues    = ∀ α. Queues (Map QueueName (Queue α))
-- | some queues are required, others can be added
data QueueName = EventQueue | LoadQueue
               | InputQueue | CustomQueue Int
-- | dynamic collection of chans
data Chans     = Chans (Map ChanName (TChan TState))
data ChanName  = CustomChan Int
-- | dynamic collection of tvars
data TVars     = ∀ α. TVars (Map TVarName (TVar (Maybe α)))
data TVarName  = WindowTVar | CustomTVar Int

-- | state holds mutable data, and the
--   current status of the whole App
data State = State { stStatus   ∷ ProgExcept
                   -- logging monadic function
                   , stLogFunc  ∷ Logger.Loc → Logger.LogSource
                                    → Logger.LogLevel → Logger.LogStr
                                    → IO ()
                   -- | the main glfw object
                   , stWindow    ∷ !(Maybe GLFW.Window) }
