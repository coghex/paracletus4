{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
-- | vulkan-specific draw loop, contains calls to GLFW and
--   runs a simple event processor to make changes to state
module Vulk where
-- the main thread is defined
-- TODO: this is the largest file and has a rediculous amount
--       of imports, it needs to be split into multiple files
import Prelude()
import UPrelude
import Control.Concurrent ( forkIO )
import Control.Monad ( forM_, when )
import Control.Monad.State.Class ( gets, modify )
import GHC.Stack ( HasCallStack )
import Luau ( luauThread )
import Prog ( MonadIO(liftIO), Prog, MonadReader(ask) )
import Prog.Event ( processEvents )
import Prog.Util ( logInfo )
import Sign.Var
    ( atomically, modifyTVar', newTVar, readTVar, writeTVar )

runVulk ∷ HasCallStack ⇒ Prog ε σ ()
runVulk = do
    logInfo "loading lua interpreter..."
    env ← ask
    _ ← liftIO $ forkIO $ luauThread env
    -- windowsizechanged is completely seperate from all other data
    windowSizeChanged ← liftIO $ atomically $ newTVar True
    logInfo "beginning paracletus..."
    -- some events must be processed in the parent thread
    processEvents
