{-# LANGUAGE Strict #-}
-- | all input events are, in their callbacks, sending this thread
--   the input in the input queue.  it is then processed here
module Prog.Input where
-- a thread to handle input
import Prelude()
import UPrelude
import Data ( KeyFunc(..), KeyMap(..), PopupType(..), Cardinal(..) )
import qualified Data.Map as Map
import qualified Data.Aeson as A
import Data.Maybe ( fromMaybe )
import Data.String ( fromString )
import qualified Data.Map as Map
import Luau.Data ( KeySettings )
import Prog.Data
    ( Env(..), QueueName(..)
    , ChanName(..) )
import Prog.Util ( readInputChan, tryReadInputQueue )
import Sign.Data
    ( Event(..), LogLevel(..), EventResult(..),
      SysAction(..), TState(..), InpCmd(..), InputEvent(..)
    , SettingsChange(..), InputState(..), InputStateChange(..) )
import Sign.Var ( atomically, readTVar, writeTVar, modifyTVar' )
import Sign.Queue
    ( readChan, tryReadChan, tryReadQueue, writeQueue )
import Sign.Util ( readChan', writeQueue'', log' )
import Control.Concurrent (threadDelay)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.IO ( openFile, hGetContents, hClose, IOMode(..) )
import qualified Vulk.GLFW as GLFW

-- | threaded recursive loop
inputThread ∷ Env → GLFW.Window → IO ()
inputThread env win = do
  log' env (LogDebug 1) "[Input] starting input thread..."
  runInputLoop env win TStart initInputState
runInputLoop ∷ Env → GLFW.Window → TState → InputState → IO ()
runInputLoop env win TStop  is = do
  tsNew ← readInputChan env
  runInputLoop env win tsNew is
runInputLoop env win TStart is = do
  start ← getCurrentTime
  tsNew ← readInputChan env
  processInputQueue env is
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = 1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  runInputLoop env win tsNew is
runInputLoop _ _ _ _ = return ()

processInputQueue ∷ Env → InputState → IO ()
processInputQueue env is = do
  rawInp ← tryReadInputQueue env
  case rawInp of
    Just inp → do
      ret ← processInput env is inp
      case ret of
        EventResultSuccess → processInputQueue env is
        EventResultError s → log' env LogError $ "[Input] " ⧺ s
    Nothing → return ()

processInput ∷ Env → InputState → InpCmd → IO (EventResult)
processInput env is (InpEvent (InputKey win key k ks mk)) = do
  processKey key ks mk is
  return EventResultSuccess
processInput env is (InpState (ISCRegisterKeys path))     = do
  keyM ← readKeySettings env path
  let is' = is { keyMap = keyM }
  return $ EventResultInputState is'
processInput env _  inpCmd
  = return $ EventResultError $ "unknown command " ⧺ show inpCmd

readKeySettings ∷ Env → String → IO KeyMap
readKeySettings env path = do
  inputSettingsFile ← openFile path ReadMode
  contents          ← hGetContents inputSettingsFile
  hClose inputSettingsFile
  let keySettings = A.decode $ fromString contents
  case keySettings of
    Just k0 → return $ createKeyMap k0
    Nothing → do
      log' env LogError $ "[Input] error decoding " ⧺ path
      return $ KeyMap Map.empty

processKey ∷ GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → InputState → IO ()
processKey key ks mk is = do
  let keyFunc = lookupKey keymap key
      keymap = keyMap is
  return ()

createKeyMap ∷ KeySettings → KeyMap
createKeyMap ks = KeyMap Map.empty

initInputState ∷ InputState
initInputState = InputState { keyMap = KeyMap Map.empty }

-- | returns the first key function with this key assigned
lookupKey ∷ KeyMap → GLFW.Key → KeyFunc
lookupKey (KeyMap keymap) key = if Map.size list > 0
  then fst $ Map.elemAt 0 list
  else KFUnknown $ show key
  where list = Map.filterWithKey (lookupInKeyMap key) keymap

lookupInKeyMap ∷ GLFW.Key → KeyFunc → [GLFW.Key] → Bool
lookupInKeyMap _  _   []     = False
lookupInKeyMap k0 kf0 (k:ks)
  | k ≡ k0    = True
  | otherwise = lookupInKeyMap k0 kf0 ks

