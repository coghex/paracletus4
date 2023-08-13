-- | logging functions for threads, helper functions to find data
module Sign.Util where
-- we defines functions to let us pass callstacks to logger
import Prelude()
import UPrelude
import GHC.Stack ( HasCallStack )
import qualified Control.Concurrent.STM as STM
import Prog ( MonadReader(ask), MonadIO(liftIO), Prog )
import Prog.Data
  ( Env(..), QueueName(..), Queues(..), QueueCmd(..), ChanName(..), Chans(..)
  , TVars(..), TVarName(..), TVarValue(..) )
import Sign.Data (LogLevel(..), Event (EventLog), TState(..) )
import Sign.Var (TVar(..), atomically, newTVar, readTVar, writeTVar)
import Sign.Queue (newQueue, writeQueue, tryReadQueue, tryReadChan, Queue(..), TChan(..))
import qualified Data.Map as M

-- | when a thread logs something we take the callstack and
--   send it over the event queue
log' ∷ HasCallStack ⇒ Env → LogLevel → String → IO ()
log' env loglevel str = case findQueue env EventQueue of
  Nothing → print "ERROR: UNKNOWN EVENT QUEUE "
  Just q0 → atomically $ writeQueue q0 $ QCEvent $ EventLog loglevel str
-- | a log for when in the Prog context
log'' ∷ HasCallStack ⇒ LogLevel → String → Prog ε σ ()
log'' loglevel str = do
  env ← ask
  liftIO $ log' env loglevel str

-- | finds a specified queue
findQueue ∷ Env → QueueName → Maybe (Queue QueueCmd)
findQueue env name = M.lookup name queues
  where Queues queues = envQueues env

-- | finds a queue in context of spits out an error
findQueue' ∷ Env → QueueName → Prog ε σ (Queue QueueCmd)
findQueue' env name = case M.lookup name queues of
  Nothing → do
    log'' LogError $ "no queue " ⧺ (show name)
    q ← liftIO $ STM.newTQueueIO
    return q
  Just q0 → return q0
  where Queues queues = envQueues env

-- | reads a queue
tryReadQueue' ∷ Env → QueueName → Prog ε σ (Maybe QueueCmd)
tryReadQueue' env queue = case findQueue env queue of
  Nothing → return Nothing
  Just q0 → liftIO $ atomically $ tryReadQueue q0

-- | writes to a queue in a prog context
writeQueue' ∷ Env → QueueName → QueueCmd → Prog ε σ ()
writeQueue' env name cmd = return ()

-- | finds a specified TState chan
findChan ∷ Env → ChanName → Maybe (TChan TState)
findChan env name = M.lookup name chans
  where Chans chans = envChans env

-- | reads a TState channel
readChan' ∷ Env → ChanName → IO (Maybe (TState))
readChan' env chan = case findChan env chan of
  Nothing → return Nothing
  Just c0 → atomically $ tryReadChan c0

-- | finds a specified TVar
findTVar ∷ Env → TVarName → Maybe (TVar (Maybe TVarValue))
findTVar env tvar = M.lookup tvar tvars
  where TVars tvars = envTVars env
-- | reads a TVar
readTVar' ∷ Env → TVarName → Prog ε σ (Maybe TVarValue)
readTVar' env tvar = case findTVar env tvar of
  Nothing → do
    log'' LogError $ "no tvar " ⧺ show tvar
    return Nothing
  Just v0 → liftIO . atomically $ readTVar v0
-- | writes a TVar
writeTVar' ∷ Env → TVarName → TVarValue → Prog ε σ ()
writeTVar' env name val = case findTVar env name of
  Nothing → do
    log'' LogError $ "no tvar " ⧺ show name
    return ()
  Just v0 → liftIO . atomically $ writeTVar v0 $ Just val
