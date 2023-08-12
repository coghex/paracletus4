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
import qualified Control.Monad.Logger.CallStack as LoggerCS
import Data.String (fromString)
import GHC.Stack ( HasCallStack, prettyCallStack, callStack )
import Prog ( Prog(..) )

-- | logInfo is like a basic print, try and avoid
logInfo ∷ HasCallStack ⇒ String → Prog ε σ ()
logInfo = LoggerCS.logInfoCS callStack ∘ fromString
