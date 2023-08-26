{-# LANGUAGE Strict #-}
-- | all input events are, in their callbacks, sending this thread
--   the input in the input queue.  it is then processed here
module Prog.Input where
-- a thread to handle input
import Prelude()
import UPrelude
import Data ( KeyFunc(..), KeyMap(..), PopupType(..), Cardinal(..), Shell(..) )
import qualified Data.Map as Map
import qualified Data.Aeson as A
import Data.Maybe ( fromMaybe )
import Data.String ( fromString )
import qualified Data.Map as Map
import Luau.Data ( InputJson(..), KeySettings(..), ShellCmd(..) )
import Prog.Data
    ( Env(..), QueueName(..)
    , ChanName(..), QueueCmd(..) )
import Prog.Mouse ( processMouse, processMouseButton )
import Prog.Util ( tryReadInputQueue )
import Sign.Data
    ( Event(..), LogLevel(..), EventResult(..), LoadCmd(..)
    , SysAction(..), TState(..), InpCmd(..), InputEvent(..)
    , SettingsChange(..), InputState(..), MouseState(..)
    , InputStateChange(..), Capture(..) )
import Sign.Var ( atomically, readTVar, writeTVar, modifyTVar' )
import Sign.Queue
    ( readChan, tryReadChan, tryReadQueue, writeQueue )
import Sign.Util ( readChan', writeQueue'', log' )
import Control.Monad ( when )
import Control.Concurrent (threadDelay)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.IO ( openFile, hGetContents, hClose, IOMode(..) )
import qualified Vulk.GLFW as GLFW

-- | threaded recursive loop
inputThread ∷ Env → GLFW.Window → IO ()
inputThread env win = do
  log' env (LogDebug 1) "[Input] starting input thread..."
  runInputLoop env win TStop initInputState
runInputLoop ∷ Env → GLFW.Window → TState → InputState → IO ()
runInputLoop env win TStop  is = do
  tsNew ← readChan' env InputChan
  case tsNew of
    Nothing  → runInputLoop env win TStop is
    Just ts0 → runInputLoop env win ts0   is
runInputLoop env win TStart s0 = do
  start ← getCurrentTime
  -- processes keys and the like
  s1 ← processInputQueue env s0
  -- processes mouse position every tick
  s2 ← processMouse env win s1
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = 1000 - usecs
  if delay > 0
    then threadDelay delay
    else return ()
  tsNew ← readChan' env InputChan
  case tsNew of
    Nothing  → runInputLoop env win TStart s2
    Just ts0 → runInputLoop env win ts0    s2
runInputLoop _ _ _ _ = return ()

processInputQueue ∷ Env → InputState → IO (InputState)
processInputQueue env is = do
  rawInp ← tryReadInputQueue env
  case rawInp of
    Just inp → do
      ret ← processInput env is inp
      case ret of
        EventResultInputState s0 → processInputQueue env s0
        EventResultSuccess       → processInputQueue env is
        EventResultError s       → do
          log' env LogError $ "[Input] " ⧺ s
          return is
    Nothing → return is

-- | processes input commands, which can be mouse clicks, keys, or state changes
processInput ∷ Env → InputState → InpCmd → IO EventResult
processInput env is (InpEvent (InputKey win key k ks mk))       = do
  processKey env key ks mk is
  return EventResultSuccess
processInput env is (InpEvent (InputMouseButton win mb mbs mk)) = do
  processMouseButton env win mb mbs mk
  return EventResultSuccess
processInput env is (InpState (ISCRegisterKeys path))           = do
  log' env (LogDebug 1) "[Input] registering keys..."
  keyM ← readKeySettings env path
  let is' = is { keyMap = keyM }
  return $ EventResultInputState is'
processInput env is (InpState (ISCCapture cap))                 = do
  let is' = is { keyCap = cap }
  return $ EventResultInputState is'
processInput env _  inpCmd
  = return $ EventResultError $ "unknown command " ⧺ show inpCmd

readKeySettings ∷ Env → String → IO KeyMap
readKeySettings env path = do
  inputSettingsFile ← openFile path ReadMode
  contents          ← hGetContents inputSettingsFile
  let keySettings = A.decode $ fromString contents
  case keySettings of
    Just (InputJson k0) → do
      hClose inputSettingsFile
      return $ createKeyMap k0
    Nothing → do
      log' env LogError $ "[Input] error decoding " ⧺ path
      hClose inputSettingsFile
      return $ KeyMap Map.empty

processKey ∷ Env → GLFW.Key → GLFW.KeyState → GLFW.ModifierKeys → InputState → IO ()
processKey env key ks mk is = do
  let keyFunc = lookupKey keymap key
      keymap = keyMap is
  --log' env (LogDebug 1) $ "processing key " ⧺ show key ⧺ ", function: " ⧺ show keyFunc
  when (ks ≡ GLFW.KeyState'Pressed) $ case keyFunc of
      KFEscape → do
        --log' env (LogDebug 1) "sending quit command"
        writeQueue'' env EventQueue $ QCEvent $ EventSys SysExit
      KFTest → do
        if keyCap is ≡ CaptureShell then
          writeQueue'' env LoadQueue $ QCLoadCmd $ LoadShell $ ShKey key mk
        else
          writeQueue'' env LoadQueue $ QCLoadCmd LoadTest
      KFShell → do
        writeQueue'' env LoadQueue $ QCLoadCmd $ LoadShell ShToggle
      keyFunc → if keyCap is ≡ CaptureShell then
          writeQueue'' env LoadQueue $ QCLoadCmd $ LoadShell $ ShKey key mk
        else
          return ()

createKeyMap ∷ KeySettings → KeyMap
createKeyMap (KeySettings kEscape kTest kShell) = km
  where km         = KeyMap km2
        km0        = Map.insert KFEscape (GLFW.getGLFWKeys kEscape) Map.empty
        km1        = Map.insert KFTest   (GLFW.getGLFWKeys kTest)   km0
        km2        = Map.insert KFShell  (GLFW.getGLFWKeys kShell)  km1

initInputState ∷ InputState
initInputState = InputState { keyMap  = KeyMap Map.empty
                            , keyCap  = CaptureNULL
                            , mouseSt = initMouseState }
initMouseState ∷ MouseState
initMouseState = MouseState { mouse1   = Nothing
                            , mouse2   = Nothing
                            , mouse3   = Nothing
                            , mousePos = (0,0) }

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

