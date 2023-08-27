{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
-- | utility functions that are needed by the prog files
module Prog.Util where
-- utility functions for the
-- anamnesis monad are defined
-- debug and logging functions,
-- and some threading functions
import Prelude()
import UPrelude
import Control.Concurrent ( forkFinally, myThreadId, throwTo )
import qualified Control.Monad.Logger.CallStack as LoggerCS
import Data.String (fromString)
import Data.Time.Clock.System
    ( SystemTime(systemNanoseconds, systemSeconds), getSystemTime )
import GHC.Stack ( HasCallStack, prettyCallStack, callStack, withFrozenCallStack )
import Graphics.Vulkan ( Int64 )
import System.Exit ( ExitCode(ExitSuccess) )
import Sign ( checkStatus )
import Sign.Data ( LogLevel(..), TState(..), InpCmd(..) )
import Sign.Except ( ExType, Exceptable, ProgExcept(ProgExcept) )
import Sign.Var ( atomically, newTVar, readTVar )
import Sign.Util (log', readChan', tryReadQueue'', modifyTVar)
import Luau.Data ( UserVar(..) )
import Prog
    ( MonadIO(liftIO), ask
    , MonadError(throwError)
    , MonadState(get)
    , Prog(..)
    , Prog' )
import Prog.Data

-- | writes data to the user data tvar
writeUDTVar ∷ UserVar → Prog ε σ ()
writeUDTVar ud = do
  env ← ask
  modifyTVar env UDTVar $ TVUD ud

-- logging functions

-- | logs the data based on its level
logCommand ∷ HasCallStack ⇒ LogLevel → String → Prog ε σ ()
logCommand (LogDebug _) = withFrozenCallStack logDebug
logCommand LogInfo      = withFrozenCallStack logInfo
logCommand LogError     = logError
logCommand LogWarn      = withFrozenCallStack logWarn
logCommand _            = withFrozenCallStack logInfo'
  where logInfo' str = logInfo $ "unknown log type: " ⧺ str

tryReadInputQueue ∷ Env → IO (Maybe InpCmd)
tryReadInputQueue env = do
  inpQ ← tryReadQueue'' env InputQueue
  case inpQ of
    Just (QCInpCmd inpCmd) → return $ Just inpCmd
    Just _                 → do
      log' env LogError
        "[Input] silly rabbit, input queue is for input commands"
      return Nothing
    Nothing                → return Nothing


-- | for c functions that have to run in the main
--   thread for as long as the program runs
occupyThreadAndFork ∷ Prog ε σ () → Prog' ε () → Prog ε σ ()
occupyThreadAndFork mainProg deputyProg = Prog $ \e s c → do
  mainThreadId ← myThreadId
  -- make new state, use same env
  threadState ← atomically $ newTVar ⌫ readTVar s
  _ ← Control.Concurrent.forkFinally
    (unProg deputyProg e threadState pure ⌦ checkStatus) $ \case
      Left  ex → throwTo mainThreadId ex
      Right () → throwTo mainThreadId ExitSuccess
  unProg mainProg e s c


-- | allocates something before returning, if
--   exception occurs, freeing does not happen
allocResource ∷ (α → Prog' ε ()) → Prog ε σ α → Prog ε σ α
allocResource free alloc = Prog $ \e s c → unProg alloc e s $ \case
  Left ex → c (Left ex)
  Right a → c (Right a) ⌦ \r → r ⚟ unProg (free a) e s pure
{-# INLINE allocResource #-}
-- | common case where we dont prepend
--   the release acton for finer control
allocResource' ∷ (α → Prog' ε ()) → Prog ε σ α
  → Prog ε σ (α, Prog ε σ ())
allocResource' free alloc = Prog $ \e s c → unProg alloc e s $ \case
  Left ex → c (Left ex)
  Right a → c (Right (a, Prog $ \e' s' c'
    → c' (Right ()) ⌦ \r → r ⚟ unProg (free a) e' s' pure))
{-# INLINE allocResource' #-}
-- | run nested continuations locally frees
--   all resources, only for side effects 
locally ∷ Prog' ε α → Prog ε σ α
locally p = Prog $ \e s c → unProg p e s pure ⌦ c
{-# INLINE locally #-}
-- | try-except-like functionality in the monad
bracket ∷ Prog ε σ α → (α → Prog ε σ β)
  → (α → Prog ε σ μ) → Prog ε σ μ
bracket before after thing = do
  a  ← before
  er ← try $ thing a
  _  ← after a
  Prog $ \_ _ → ($ er)
{-# INLINE bracket #-}
-- | sequence the action if we can
finally ∷ Prog ε σ α → Prog ε σ β → Prog ε σ α
finally a sequal = do
  er ← try a
  _  ← sequal
  Prog $ \_ _ → ($ er)
-- | attempt to run action monadically
{-# INLINE finally #-}
try ∷ Prog ε σ α → Prog ε σ (Either ProgExcept α)
try a = Prog $ \e s c → unProg a e s $ c . Right
-- | simple loop that checks to see if the status has changed
--   and exits the loop
{-# INLINE try #-}
loop ∷ Prog' ε LoopControl → Prog ε σ ()
loop action = do
  status ← locally action
  if status ≡ ContinueLoop then loop action else return ()
-- | a method to return time since the start of the program
getTime :: Prog ε σ Double
getTime = do
  now <- liftIO getSystemTime
  start <- stStartT <$> get
  let deltaSeconds      = systemSeconds now - systemSeconds start
      deltaNS :: Int64  = fromIntegral (systemNanoseconds now)
                            - fromIntegral (systemNanoseconds start)
      seconds :: Double = fromIntegral deltaSeconds
                            + fromIntegral deltaNS / 1e9
  return seconds

-- | debugging flags, so the preprocesser only has to work here
isDev ∷ Bool
#ifdef DEVELOPMENT
isDev = True
#else
isDev = False
#endif
{-# INLINE isDev #-}
-- | forces strictness
inDev ∷ Applicative m ⇒ m () → m ()
#ifdef DEVELOPMENT
inDev = id
#else
inDev = const (pure ())
#endif
{-# INLINE inDev #-}
-- logging functions

-- | logDebug compiles to nothing if dev flag is false
logDebug ∷ HasCallStack ⇒ String → Prog ε σ ()
#ifdef DEVELOPMENT
logDebug = LoggerCS.logDebugCS callStack ∘ fromString
#else
logDebug = const $ pure ()
#endif
{-# INLINE logDebug #-}
-- | logInfo is like a basic print, try and avoid
logInfo ∷ HasCallStack ⇒ String → Prog ε σ ()
logInfo = LoggerCS.logInfoCS callStack ∘ fromString
{-# INLINE logInfo #-}
-- | logWarn is telling user something is not right
logWarn ∷ HasCallStack ⇒ String → Prog ε σ ()
logWarn = LoggerCS.logWarnCS callStack ∘ fromString
{-# INLINE logWarn #-}
-- | logError is telling the user something is impossible
logError ∷ HasCallStack ⇒ String → Prog ε σ ()
logError msg = LoggerCS.logErrorCS callStack ∘ fromString $ msg ⧺ "\n" ⧺ prettyCallStack callStack ⧺ "\n"
{-# INLINE logError #-}
-- | logExcept will actually kill the whole program and print a callstack
logExcept ∷ (Exceptable ς, HasCallStack)
  ⇒ ς → ExType → String → Prog ε σ α
logExcept ret exType msg = throwError
  $ ProgExcept (Just ret) exType (msg ⧺ "\n" ⧺ prettyCallStack callStack ⧺ "\n")

