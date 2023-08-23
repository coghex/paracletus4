{-# LANGUAGE Strict #-}
-- a thread which keeps track of time
module Time where
-- a thread to help recreate the swapchain
import Prelude ()
import UPrelude
import Data ( PrintArg(PrintNULL), Color(..), ID(..), Shell(..) )
import Data.Aeson as A
import Data.Maybe ( fromMaybe )
import Data.List.Split ( splitOn )
import qualified Data.Map as Map
import Data.String ( fromString )
import Load.Data
import Load.Util ( emptyTiles )
import Prog.Data
import Prog.Buff ( generateDynData )
import Sign.Data
import Sign.Log
import Time.Data
import Vulk.Calc ( calcVertices )
import Vulk.Data ( Verts(Verts) )
import Util ( newID )
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class ( liftIO, MonadIO(..) )
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Vulk.GLFW as GLFW
import System.Log.FastLogger (LogType'(LogStdout))
import System.IO ( openFile, hGetContents, hClose, IOMode(..) )

-- | threaded loop provides work so main thread doesnt stutter
timeThread ∷ Env → IO ()
timeThread env = do
  logger ← makeDefaultLogger env (LogStdout 4096) (LogDebug 1)
  runLog logger $ runTimeLoop initTS TStop
  where initTS = initTimeState
runTimeLoop ∷ (MonadLog μ,MonadFail μ) ⇒ TimeState → TState → LogT μ ()
runTimeLoop ts TStop = do
  -- kind of confusing naming but the timer has a timer
  tsNew ← readTimeTimerBlocked
  runTimeLoop ts tsNew
runTimeLoop ts TStart = do
  start ← liftIO getCurrentTime
  timerState ← readTimeTimer
  tsNew ← case timerState of
    Nothing → return TStart
    Just x  → return x
  ts0 ← runTimers ts
  ts' ← processCommands ts0
  end ← liftIO getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = 1000 - usecs
  if delay > 0
    then liftIO $ threadDelay delay
    else return ()
  runTimeLoop ts' tsNew
-- pause not needed for this timer
runTimeLoop _ TPause = return ()
runTimeLoop _ TNULL  = return ()

-- | command queue processed once per loop
processCommands ∷ (MonadLog μ,MonadFail μ) ⇒ TimeState → LogT μ TimeState
processCommands ts = do
  mcmd ← readTimeCommand
  case mcmd of
    Just cmd → do
      ret ← processCommand ts cmd
      case ret of
        -- if command success keep processing commands
        TimeResultSuccess   → processCommands ts
        TimeResultState ts' → processCommands ts'
        TimeResultError str     → do
          log' LogError $ "time command error: " ⧺ str
          return ts
        _                       → do
          log' LogError "unknown time command result"
          return ts
    Nothing → return ts

-- | this is the case statement for processing load commands
processCommand ∷ (MonadLog μ,MonadFail μ)
  ⇒ TimeState → TimeCmd → LogT μ TimeResult
processCommand ts cmd = case cmd of
  TCState timer st → do
    let ts' = ts { tsTimers = setTimerState (tsTimers ts) timer st }
    return $ TimeResultState ts'
  cmd → do
    return $ TimeResultError $ "unknown time cmd " ⧺ show cmd
  _ → return TimeResultSuccess

-- | finds the right timer and sets its state
setTimerState ∷ [Timer] → TimerName → TState → [Timer]
setTimerState []             _    _  = []
setTimerState (timer:timers) name st = timer' : timers
  where timer' = if timerName timer ≡ name
                   then timer { timerState = st }
                   else timer

-- | increments timers if they need to be
runTimers ∷ (MonadLog μ,MonadFail μ)
  ⇒ TimeState → LogT μ TimeState
runTimers ts = do
  timers ← mapM runTimer $ tsTimers ts
  return $ ts { tsTimers = timers }
runTimer ∷ (MonadLog μ,MonadFail μ)
 ⇒ Timer → LogT μ Timer
runTimer timer = case timerState timer of
  TStart → do
    if timerVal timer ≥ timerInterval timer then do
      sendTimer $ ShellCursorTimer
      return timer { timerVal = 0 }
    else do
      return timer { timerVal = (timerVal timer) + 1 }
  TStop → return timer { timerVal = 0 }
  _      → return timer

-- | initial time state
initTimeState ∷ TimeState
initTimeState = TimeState TSNULL 0 initTimers
-- | we start with an initial timer to handle the shell cursor
initTimers ∷ [Timer]
initTimers = [shellcursortimer]
  where shellcursortimer = Timer TStop ShellCursorTimer 0 500

-- | proccesses the timers from the load thread,
--   note this is run in the LOAD THREAD
processTimer ∷ (MonadLog μ, MonadFail μ)
  ⇒ DrawState → TimerName → LogT μ LoadResult
processTimer ds ShellCursorTimer = return $ LoadResultDrawState ds'
  where ds'      = ds { dsShell  = newshell
                      , dsStatus = DSSReload }
        newshell = (dsShell ds) { shCursSt = not (shCursSt (dsShell ds)) }
processTimer ds timername        = return $ LoadResultError str
  where str = "unknown timer " ⧺ show timername
