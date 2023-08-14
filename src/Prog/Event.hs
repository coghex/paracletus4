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
import Prog.Data (Env(..), State(..), QueueCmd(..), QueueName(..))
import Prog.Util ( logCommand )
import Sign.Data
    ( Event(..), LogLevel(..), SysAction(..), InpCmd(..) )
import Sign.Except ( ExType(ExVulk) )
import Sign.Var ( atomically, modifyTVar' )
import Sign.Util ( tryReadQueue', writeQueue', log'')

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
  EventSys sysEvent   → processSysEvent sysEvent
  EventSys SysExit    → do
    logCommand (LogDebug 1) "quitting..."
    st ← get
    case stWindow st of
      Just win → liftIO $ GLFW.setWindowShouldClose win True
      Nothing  → liftIO exitSuccess
  EventLog level str  → logCommand level str
  EventInput inpEvent → writeQueue' InputQueue $ QCInpCmd $ InpEvent inpEvent
  _                   → log'' LogError $ "Unknown event: " ⧺ show event
processEvent _               = return ()

processSysEvent ∷ SysAction → Prog ε σ ()
processSysEvent SysExit = do
  st ← get
  case stWindow st of
    Just win → liftIO $ GLFW.setWindowShouldClose win True
    Nothing  → liftIO exitSuccess
processSysEvent event   = log'' LogWarn $ "Unknown sysaction: " ⧺ show event
