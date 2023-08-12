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
import Data ( Color(..) )
import Prog ( Prog(unProg) )
import Prog.Data ( Env(..), State(..), Queues(..), Chans(..)
                 , TVars(..), ProgResult(..) )
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
  let env = Env { envQueues = queues
                , envChans  = chans
                , envTVars  = tvars
                , envLuaSt  = luaSt }
      queues = Queues Map.empty
      chans  = Chans Map.empty
      tvars  = TVars Map.empty
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
  let ref = ProgExcept (Just ProgSuccess) ExProg ""
  -- the logger provides multiple levels of warnings/info
  lf ← Logger.runStdoutLoggingT $ Logger.LoggingT pure
  -- state is accessed transactionally
  atomically $ newTVar State { stStatus    = ref
                             , stLogFunc   = lf
                             , stWindow    = Nothing }
