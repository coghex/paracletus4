{-# LANGUAGE StrictData #-}
-- | initialization of many things, including state,
--   env, drawsstate, inputsttate, settings, and
--   the keymapping, among others...
module Prog.Init
  ( runProg ) where
-- initialization of env and state occurs
import Prelude()
import UPrelude
import qualified Control.Monad.Logger.CallStack as Logger
import Data ( Color(..), FPS(..) )
import Prog ( Prog(unProg) )
import Prog.Data ( Env(..), State(..), ProgResult(..), QueueName(..), ChanName(..),
                   Queues(..), Chans(..), TVars(..), ReloadState(..), Settings(..),
                   TVarName(..) )
import Sign.Data ( Event(..), LoadCmd(..), InpCmd(..) )
import Sign.Except ( ExType(ExProg), ProgExcept(ProgExcept) )
import Sign.Queue ( newQueue, newTChan )
import Sign.Var ( atomically, newTVar, TVar )
import qualified Data.Map as Map
import Data.Time.Clock.System ( getSystemTime )
import qualified HsLua as Lua
import GHC.Stack ( HasCallStack) -- , prettyCallStack, callStack )

-- | the entire monad is unraveled here, after the init functions
runProg ∷ HasCallStack ⇒ (Either ProgExcept α → IO σ)
  → Prog ε σ α → IO σ
runProg c p = do
  (envchan,env) ← initEnv
  st            ← initState env
  unProg p envchan st c

-- | read-only env is best for channels, queues, and other pointers
initEnv ∷ IO (TVar Env, Env)
initEnv = do
  luaSt ← Lua.newstate
  -- event queues handles events from main thread, the
  -- event thread commands are for the main draw thread
  eventQ   ← newQueue
  -- load queue calculates then delivers verticies/indicies
  -- to the draw thread from the drawState
  loadQ    ← newQueue
  -- input thread tracks the mouse and processes input
  inpQ     ← newQueue
  -- time thread sends commands in regular intervals
  timeQ    ← newQueue
  -- luau thead commands control data exchange to lua
  luauQ    ← newQueue
  -- channels that contain semaphores for each thread
  luaCh    ← newTChan
  loadCh   ← newTChan
  inpCh    ← newTChan
  timeCh   ← newTChan
  -- vert TVar keeps verticies in a cache so when we only
  -- recalculate if we explicitly ask for it
  vertsTV  ← atomically $ newTVar Nothing
  -- same for dynamic data, there will be lots of it
  dynsTV   ← atomically $ newTVar Nothing
  -- we need to keep track of how large the font is since we are
  -- putting it first in the list of textures and different fonts
  -- with different sizes can be loaded
  fsTV     ← atomically $ newTVar Nothing
  -- we also need to keep track of the font hinting data
  fmTV     ← atomically $ newTVar Nothing
  -- other data associated with each font
  fTV      ← atomically $ newTVar Nothing
  -- Tvar specifically for returning object IDs to lua
  idTV     ← atomically $ newTVar Nothing
  -- Tvar specifically for returning data back to lua
  udTV     ← atomically $ newTVar Nothing
  let env = Env { envQueues = Queues queues5
                , envChans  = Chans chans4
                , envTVars  = TVars tvars7
                , envLuaSt  = luaSt }
      queues0 = Map.empty
      queues1 = Map.insert EventQueue eventQ  queues0
      queues2 = Map.insert LoadQueue  loadQ   queues1
      queues3 = Map.insert InputQueue inpQ    queues2
      queues4 = Map.insert TimeQueue  timeQ   queues3
      queues5 = Map.insert LuauQueue  luauQ   queues4
      chans0  = Map.empty
      chans1  = Map.insert LuaChan    luaCh   chans0
      chans2  = Map.insert LoadChan   loadCh  chans1
      chans3  = Map.insert InputChan  inpCh   chans2
      chans4  = Map.insert TimeChan   timeCh  chans3
      tvars0  = Map.empty
      tvars1  = Map.insert VertsTVar    vertsTV tvars0
      tvars2  = Map.insert DynsTVar     dynsTV  tvars1
      tvars3  = Map.insert FontSizeTVar fsTV    tvars2
      tvars4  = Map.insert FontMapTVar  fmTV    tvars3
      tvars5  = Map.insert FontsTVar    fTV     tvars4
      tvars6  = Map.insert IDTVar       idTV    tvars5
      tvars7  = Map.insert UDTVar       udTV    tvars6
  -- and env that can be accessed transactionally
  envChan ← atomically $ newTVar env
  -- we return both so that initState doesnt need to load the TVar
  return (envChan, env)

-- | state is bes for things that change often and need to be accessed
--   often verts and dyns, and some other more time critical things are
--   kepts as transactional data referenced in the env.
initState ∷ Env → IO (TVar State)
initState _   = do
  -- the status handles errors
  let ref      = ProgExcept (Just ProgSuccess) ExProg ""
      settings = Settings $ Just 60
  -- initial settings before we even check any files
  -- the logger provides multiple levels of warnings/info
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  -- the system time marks the start of execution
  st ← getSystemTime
  -- state is accessed transactionally
  atomically $ newTVar State { stStatus    = ref
                             , stLogFunc   = lf
                             , stWindow    = Nothing
                             , stReload    = RSNULL
                             , stLoaded    = False
                             , stSettings  = settings
                             , stTextures  = []
                             , stFont      = []
                             , stStartT    = st
                             , stFPS       = FPS 60.0 60 True
                             , stTick      = Nothing
                             , stCamera    = (0,0,-1) }
