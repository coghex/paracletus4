-- | logging functions for threads, helper functions to find data
module Sign.Util where
-- we defines functions to let us pass callstacks to logger
import Prelude()
import UPrelude
import GHC.Stack ( HasCallStack )
import Prog ( MonadReader(ask), MonadIO(liftIO), Prog )
import Prog.Data
  ( Env(..), QueueName(..), Queues(..), QueueCmd(..) )
import Sign.Data (LogLevel(..), Event (EventLog))
import Sign.Var (atomically)
import Sign.Queue (writeQueue, tryReadQueue, Queue(..))
import qualified Data.Map as M

-- | when a thread logs something we take the callstack and
--   send it over the event queue
log' ∷ HasCallStack ⇒ Env → LogLevel → String → IO ()
log' env loglevel str = case findQueue env EventQueue of
  Nothing → print "ERROR: UNKNOWN QUEUE "
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

-- | reads a queue
tryReadQueue' ∷ Env → QueueName → Prog ε σ (Maybe QueueCmd)
tryReadQueue' env queue = case findQueue env queue of
  Nothing → return Nothing
  Just q0 → liftIO $ atomically $ tryReadQueue q0
