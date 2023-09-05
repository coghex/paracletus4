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
import Luau.Data ( UserVar(..) )
import Prog
    ( MonadIO(liftIO), Prog, MonadReader(ask), MonadState(get) )
import Prog.Data (Env(..), State(..), ReloadState(..), QueueCmd(..), QueueName(..),
                  TVarName(..), TVarValue(..))
import Prog.Util ( logCommand, writeUDTVar )
import Sign.Data
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
    logCommand (LogDebug 1) "[Vulk] quitting..."
    st ← get
    case stWindow st of
      Just win → liftIO $ GLFW.setWindowShouldClose win True
      Nothing  → liftIO exitSuccess
  EventSys sysEvent    → log'' LogWarn $ "Unknown sysaction: " ⧺ show event
  EventLog level str   → logCommand level str
  EventInput (InputMouseScroll _   _ y) → do
    st ← get
    let (cx,cy,cz) = stCamera st
        cz'        = min -0.1 $ max -10 $ cz - (0.1*realToFrac y)
    modify $ \s → s { stCamera = (cx,cy,cz') }
  EventInput inpEvent  → writeQueue' InputQueue $ QCInpCmd $ InpEvent inpEvent
  EventLoadFont font → modify $ \s → s { stFont   = stFont s ⧺ [font] }
  EventTextures texmap → do
    modify $ \s → s { stTextures = texmap }
  EventGet gc → getData gc
  EventTest → do
    st ← get
    logCommand LogInfo $ show $ stLoaded st
  _                   → log'' LogError $ "Unknown event: " ⧺ show event
processEvent _               = return ()

getData ∷ GetCommand → Prog ε σ ()
getData GCWindow = do
  win ← gets stWindow
  case win of
    Nothing → do
      logCommand LogError "no window in state"
      writeUDTVar UVNULL
    Just w0 → writeUDTVar $ UVWindow w0
getData gc       = logCommand LogWarn $ "unknown get command " ⧺ show gc
