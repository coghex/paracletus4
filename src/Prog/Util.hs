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
import Prog ( Prog(..), MonadError(throwError) )
import Sign.Data ( LogLevel(..) )
import Sign.Except ( ExType, Exceptable, ProgExcept(ProgExcept) )

-- logging functions

-- | logs the data based on its level
logCommand ∷ LogLevel → String → Prog ε σ ()
logCommand (LogDebug _) = logDebug
logCommand LogInfo      = logInfo
logCommand LogError     = logError
logCommand LogWarn      = logWarn
logCommand _            = logInfo'
  where logInfo' str = logInfo $ "unknown log type: " ⧺ str

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
logError = LoggerCS.logErrorCS callStack ∘ fromString
{-# INLINE logError #-}
-- | logExcept will actually kill the whole program and print a callstack
logExcept ∷ (Exceptable ς, HasCallStack)
  ⇒ ς → ExType → String → Prog ε σ α
logExcept ret exType msg = throwError
  $ ProgExcept (Just ret) exType (msg ⧺ "\n" ⧺ prettyCallStack callStack ⧺ "\n")

