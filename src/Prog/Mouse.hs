{-# LANGUAGE Strict #-}
-- | mouse related functionality, inluding scroll,
--   click, and positional monitoring 
module Prog.Mouse where
-- mouse input is handled
import Prelude()
import UPrelude
import Prog.Data (Env(..))
import Sign.Data (InputState(..), MouseState(..), LogLevel(..))
import Sign.Util ( log' )
import qualified Vulk.GLFW as GLFW

-- | processes mouse position every frame
processMouse ∷ Env → GLFW.Window → InputState → IO InputState
processMouse env win is = do
  pos ← GLFW.getCursorPos win
  let newms = (mouseSt is) { mousePos = pos }
  return is { mouseSt = newms }

-- | processes mouse button clicks
processMouseButton ∷ Env → GLFW.Window → GLFW.MouseButton
  → GLFW.MouseButtonState → GLFW.ModifierKeys → IO ()
processMouseButton env win mb mbs mk = do
  pos ← GLFW.getCursorPos win
  log' env LogInfo $ show mb ⧺ ": " ⧺ show pos
  return ()
