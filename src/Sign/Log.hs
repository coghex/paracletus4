{-# LANGUAGE OverloadedStrings, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, Strict, GeneralizedNewtypeDeriving, ConstraintKinds, FlexibleContexts, MonoLocalBinds, CPP, ImplicitParams #-}
-- | logging functions for threads
module Sign.Log where
-- we defines functions to let us pass callstacks to logger
-- it has been hacked to provide an interface to the main thread

import Prelude()
import UPrelude
import Control.Monad.IO.Class
import Control.Monad.Morph
import Control.Monad.Reader
import System.Log.FastLogger
import Data ( ID )
import Load.Data ( DynData(..), TextureMap(..), Tex(..) )
import Prog.Data ( Env(..), ChanName(..), QueueName(..), QueueCmd(..)
                 , TVarName(..), TVarValue(..))
import Sign.Data ( LogLevel(..), Event(..), TState(..), LoadCmd(..)
                 , SysAction(..), SettingsChange(..) )
import Sign.Var ( atomically )
import Sign.Queue ( writeQueue, readChan, tryReadChan, writeChan )
import Sign.Util ( readChan', writeQueue'', tryReadQueue'', readTVar''
                 , writeTVar'', modifyTVar'' )
import Vulk.Font (TTFData(..))
import Vulk.Data (Verts(..))
import qualified Vulk.GLFW as GLFW

-- | monadic boilerplate logger from simple-log package on hackage
data Log = Log { logLevel  ∷ LogLevel
               , env       ∷ Env
               , formatter ∷ LogLevel → String → LogStr
               , logFunc   ∷ LogStr → IO ()
               , cleanUp   ∷ IO () } | LogIO

-- | exposes MonadLog class
class (MonadIO μ) ⇒ MonadLog μ where
  askLog   ∷ μ Log
  localLog ∷ (Log → Log) → μ α → μ α

instance {-# OVERLAPPABLE #-} (MonadLog μ, MonadTrans τ, MonadLog IO, MonadFail μ
                              , MFunctor τ, MonadIO (τ μ))
                                  ⇒ MonadLog (τ μ) where
  askLog      = lift askLog
  localLog fn = hoist $ localLog fn

-- | a logger with ReaderT state to remember the event queue
newtype LogT μ α = LogT { runLogT ∷ ReaderT Log μ α }
  deriving (Functor, Applicative, Monad, MonadFail, MonadIO, MonadReader Log)

instance MonadTrans LogT where
  lift = LogT ∘ lift

instance (MonadIO μ) ⇒ MonadLog (LogT μ) where
  askLog      = LogT ask
  localLog fn = LogT ∘ local fn ∘ runLogT
instance (MonadLog IO) where
  askLog      = return LogIO
  localLog fn = localLog fn

makeLogger ∷ (MonadIO μ) ⇒ Env → (LogLevel → String → LogStr) → LogType → LogLevel → μ Log
makeLogger env fmt typ lvl = liftIO $ do
  (fl,cl) ← newFastLogger typ
  return $ Log lvl env fmt fl cl

makeDefaultLogger ∷ (MonadIO μ) ⇒ Env → LogType → LogLevel → μ Log
makeDefaultLogger env = makeLogger env defaultFormatter

defaultFormatter ∷ LogLevel → String → LogStr
defaultFormatter _ = toLogStr

-- | runs the reader monad
runLog ∷ (MonadIO μ) ⇒ Log → LogT μ α → μ α
runLog l m = runReaderT (runLogT m) l

-- *** utility functions

-- | raw log, meant for use with the logging functions
log' ∷ (MonadLog μ, MonadFail μ) ⇒ LogLevel → String → μ ()
log' lvl msg = do
  (Log fil env _   _   _) ← askLog
  when (lvlbelow fil lvl) $ liftIO $ do
    liftIO $ writeQueue'' env EventQueue $ QCEvent $ EventLog lvl msg
    -- this line would use the fast logger, this may be better to use
--    ( fun ∘ toLogStr ) (fmt lvl msg)
-- | checks that the message level is below verbosity
lvlbelow ∷ LogLevel → LogLevel → Bool
lvlbelow (LogDebug n) (LogDebug m) = n ≥ m -- debug level
lvlbelow (LogDebug _) _            = True
lvlbelow _            (LogDebug _) = False
lvlbelow LogInfo      LogInfo      = True
lvlbelow _            LogInfo      = False
lvlbelow _            _            = True

-- | returns nothing if load channel is empty
readTimer ∷ (MonadLog μ, MonadFail μ) ⇒ μ (Maybe TState)
readTimer = do
  (Log _   env _   _   _) ← askLog
  liftIO $ readChan' env LoadChan

-- | hangs execution until it reads something
readTimerBlocked ∷ (MonadLog μ, MonadFail μ) ⇒ μ TState
readTimerBlocked = do
  (Log _   env _   _   _) ← askLog
  res ← liftIO $ readChan' env LoadChan
  case res of
    Nothing  → do
      liftIO $ print "ERROR: NO TIMER CHAN"
      return TStop
    Just res → return res

-- | returns nothing if load queue is empty
readCommand ∷ (MonadLog μ, MonadFail μ) ⇒ μ (Maybe LoadCmd)
readCommand = do
  (Log _   env _   _   _) ← askLog
  lc ← liftIO $ tryReadQueue'' env LoadQueue
  case lc of
    Nothing              → return Nothing
    Just (QCLoadCmd lc0) → return $ Just lc0
    Just badloadcmd      → do
      log' LogError $ "bad load command " ⧺ show badloadcmd
      return Nothing

-- | writes to the ID chan
writeIDChan ∷ (MonadLog μ, MonadFail μ) ⇒ ID → μ ()
writeIDChan id = do
  (Log _   env _   _   _) ← askLog
  liftIO $ atomically $ writeChan (envIDChan env) id

-- | sends a syscommand over the event queue
sendSys ∷ (MonadLog μ, MonadFail μ) ⇒ SysAction → μ ()
sendSys sa = do
  (Log _   env _   _   _) ← askLog
  liftIO $ writeQueue'' env EventQueue $ QCEvent $ EventSys sa
-- | sends the list of textures to the event queue
sendTextures ∷ (MonadLog μ, MonadFail μ) ⇒ [(String,Tex)] → μ ()
sendTextures tm = do
  (Log _   env _   _   _) ← askLog
  liftIO $ writeQueue'' env EventQueue $ QCEvent $ EventTextures $ map (tfp ⊚ snd) tm
-- | sends a syscommand over the event queue
sendTest ∷ (MonadLog μ, MonadFail μ) ⇒ μ ()
sendTest = do
  (Log _   env _   _   _) ← askLog
  liftIO $ writeQueue'' env EventQueue $ QCEvent $ EventTest
-- | reads a tvar
readTVar ∷ (MonadLog μ, MonadFail μ) ⇒ TVarName → μ (Maybe TVarValue)
readTVar tvar = do
  (Log _   env _   _   _) ← askLog
  liftIO $ readTVar'' env tvar
-- | writes a tvar
writeTVar ∷ (MonadLog μ, MonadFail μ) ⇒ TVarName → TVarValue → μ ()
writeTVar tvar val = do
  (Log _   env _   _   _) ← askLog
  liftIO $ writeTVar'' env tvar val
-- | modifies a tvar
modifyTVar ∷ (MonadLog μ, MonadFail μ) ⇒ TVarName → TVarValue → μ ()
modifyTVar tvar val = do
  (Log _   env _   _   _) ← askLog
  liftIO $ modifyTVar'' env tvar val
  
-- | reads the font size tvar
readFontSize ∷ (MonadLog μ, MonadFail μ) ⇒ μ Int
readFontSize = do
  -- first we check to see if we loaded a font
  fontSize ← readTVar FontSizeTVar
  case fontSize of
    Nothing         → return 0
    Just (TVInt fs) → return fs
    Just _          → return 0

-- | reads the current key layout
readFontMapM ∷ (MonadLog μ, MonadFail μ) ⇒ LogT μ [TTFData]
readFontMapM = do
  (Log _   env _   _   _) ← askLog
  fontMap ← readTVar FontMapTVar
  case fontMap of
    Nothing              -> return []
    Just (TVFontMap tvv) -> return tvv

