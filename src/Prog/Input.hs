{-# LANGUAGE Strict #-}
-- | all input events are, in their callbacks, sending this thread
--   the input in the input queue.  it is then processed here
module Prog.Input where
-- a thread to handle input
import Prelude()
import UPrelude
import Data ( KeyFunc(..), KeyMap(..), PopupType(..), Cardinal(..) )
import Data.Maybe ( fromMaybe )
import Prog.Data
    ( Env(..)
    , ChanName(..) )
import Sign.Data
    ( Event(..), LogLevel(..),
      SysAction(..), TState(..)
    , SettingsChange(..), InputStateChange(..) )
import Sign.Var ( atomically, readTVar, writeTVar, modifyTVar' )
import Sign.Queue
    ( readChan, tryReadChan, tryReadQueue, writeQueue )
import Sign.Util ( readChan', writeQueue'', log' )
import Control.Concurrent (threadDelay)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Vulk.GLFW as GLFW

-- | threaded recursive loop
inputThread ∷ Env → GLFW.Window → IO ()
inputThread env win = do
  log' env (LogDebug 1) "[Input] starting input thread..."
  runInputLoop env win TStart
runInputLoop ∷ Env → GLFW.Window → TState → IO ()
runInputLoop env win TStop = do
  tsNew' ← readChan' env InputChan
  tsNew ← case tsNew' of
            Nothing → do
              log' env LogError "[Input] no input channel"
              return TNULL
            Just t0 → return t0
  runInputLoop env win tsNew
runInputLoop env win TStart = do
  runInputLoop env win TStop
runInputLoop _ _ _ = return ()
