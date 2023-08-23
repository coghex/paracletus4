-- | data kept in the timer thread
module Time.Data where
import Prelude()
import UPrelude
import Sign.Data ( TState(..), TimerName(..) )

data TimeState = TimeState { tsStatus ∷ TSStatus
                           , tsCount  ∷ Int
                           , tsTimers ∷ [Timer] } deriving (Show, Eq)
data TimeCmd = TCState TimerName TState | TCNULL deriving (Show, Eq)
data TimeResult = TimeResultSuccess | TimeResultState TimeState
                | TimeResultError String deriving (Show, Eq)
data TSStatus = TSSuccess | TSError String
              | TSNULL deriving (Show, Eq)
data Timer = Timer { timerState    ∷ TState
                   , timerName     ∷ TimerName
                   , timerVal      ∷ Int
                   , timerInterval ∷ Int } deriving (Show, Eq)
