{-# LANGUAGE Strict #-}
-- | events are processed in the parent thread so
--   as little work as possible is done here
module Prog.Event where
-- events and exceptions are handled
import Prelude()
import UPrelude
import Control.Monad.State.Class (modify,gets)
import System.Exit (exitSuccess)
import qualified Vulk.GLFW as GLFW
import Prog
    ( MonadIO(liftIO), Prog, MonadReader(ask), MonadState(get) )
import Prog.Data (Env(..), State(..), ReloadState(..), QueueCmd(..), QueueName(..),
                  TVarName(..), TVarValue(..))
import Prog.Util ( logCommand )
import Sign.Data
    ( Event(..), LogLevel(..), SysAction(..), InpCmd(..), LoadCmd(..) )
import Sign.Except ( ExType(ExVulk) )
import Sign.Var ( atomically, modifyTVar' )
import Sign.Util ( tryReadQueue', writeQueue', log'', writeTVar', readTVar')

-- | reads event channel, then exectutes events recursively
processEvents ∷ Prog ε σ ()
processEvents = do
  env ← ask
  event ← tryReadQueue' env EventQueue
  case event of
    Just e → do
      processEvent e
      processEvents
    Nothing → return ()
-- | case statement on each event, these are mostly callbacks
--   since we want as little work as possible here
processEvent ∷ QueueCmd → Prog ε σ ()
processEvent (QCEvent event) = case event of
  EventSys SysReload   → modify $ \s → s { stReload = RSReload }
  EventSys SysRecreate → modify $ \s → s { stReload = RSRecreate }
  EventSys SysExit     → do
    logCommand (LogDebug 1) "quitting..."
    st ← get
    case stWindow st of
      Just win → liftIO $ GLFW.setWindowShouldClose win True
      Nothing  → liftIO exitSuccess
  EventSys sysEvent    → log'' LogWarn $ "Unknown sysaction: " ⧺ show event
  EventLog level str   → logCommand level str
  EventInput inpEvent  → writeQueue' InputQueue $ QCInpCmd $ InpEvent inpEvent
  EventLoadFont fp → modify $ \s → s { stFont   = Just fp
                                     , stReload = RSRecreate }
  EventTextures texmap → do
    modify $ \s → s { stTextures = texmap
                    , stReload   = RSRecreate }
  EventTest → do
    env ← ask
    dyns ← readTVar' env DynsTVar
    case dyns of
      Nothing          → logCommand LogWarn "no dyns"
      Just (TVDyns d0) → logCommand (LogDebug 1) $ "len: " ⧺ show d0
      Just _           → logCommand LogWarn "junk in dyns"
  _                   → log'' LogError $ "Unknown event: " ⧺ show event
processEvent _               = return ()
