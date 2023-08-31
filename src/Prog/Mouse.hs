{-# LANGUAGE Strict #-}
-- | mouse related functionality, inluding scroll,
--   click, and positional monitoring 
module Prog.Mouse where
-- mouse input is handled
import Prelude()
import UPrelude
import Data.List.Split ( splitOn )
import Prog.Data
import Sign.Data
import Sign.Util ( log', writeQueue'' )
import Vulk.Font ( indexTTFData, TTFData(..), GlyphMetrics(..) )
import qualified Vulk.GLFW as GLFW

-- | processes mouse position every frame
processMouse ∷ Env → GLFW.Window → InputState → IO InputState
processMouse env win is = do
  pos ← GLFW.getCursorPos win
  siz ← GLFW.getWindowSize win
  let newms = (mouseSt is) { mousePos = pos }
  return is { mouseSt = newms }

-- | processes mouse button clicks
processMouseButton ∷ Env → InputState → GLFW.Window → GLFW.MouseButton
  → GLFW.MouseButtonState → GLFW.ModifierKeys → IO InputState
processMouseButton env is win mb mbs mk = do
  pos ← GLFW.getCursorPos win
  siz ← GLFW.getWindowSize win
  let elems = findElemsUnder pos' $ inputElems is
      pos'  = normalizePos pos siz
  sendClick env elems
  return is

-- | sends data to the load thread corresponding
--   to the first input elem in the list
sendClick ∷ Env → [InputElem] → IO ()
sendClick _   []            = return ()
sendClick env ((IEButt butt):es) = do
  writeQueue'' env LoadQueue $ QCLoadCmd $ LoadInput $ LIButton butt
sendClick env (_:es)        = sendClick env es

-- | returns the list of input elems at the specified position
findElemsUnder ∷ (Double,Double) → [InputElem] → [InputElem]
findElemsUnder _   []     = []
findElemsUnder pos (e:es)
  | elemUnder pos e = e : findElemsUnder pos es
  | otherwise       = findElemsUnder pos es

-- | returns true if the element is under the position
elemUnder ∷ (Double,Double) → InputElem → Bool
elemUnder (mx,my) (IEButt (Button _ (x,y) (w,h)))
  = (abs(mx - x - 1.5) < w)
  ∧ (abs(my - y) < (0.5*h))
elemUnder _     IENULL                      = False

-- | turns GLFW mouse coordinates into Vulkan coordinates
normalizePos ∷ (Double,Double) → (Int,Int) → (Double,Double)
normalizePos (x,y) (w,h) = (x',y')
  where x' = (x / 32.0) - w'
        y' = h' - (y / 32.0)
        w' = fromIntegral w / 64.0
        h' = fromIntegral h / 64.0
-- | figure out what size the textbox should be
calcTextBoxSize ∷ String → [TTFData] → (Double,Double)
calcTextBoxSize str ttfdat
  = (max 1 (calcTBWidth str ttfdat)
    ,fromIntegral (length (splitOn ['\n'] str)))
calcTBWidth ∷ String → [TTFData] → Double
calcTBWidth []        _      = 0.1
calcTBWidth (' ':str) ttfdat = 0.1 + calcTBWidth str ttfdat
calcTBWidth (ch:str)  ttfdat = case indexTTFData ttfdat ch of
  Nothing → calcTBWidth str ttfdat
  Just d0 → chA + calcTBWidth str ttfdat
    where (TTFData _ _ (GlyphMetrics _ _ _ _ chA)) = d0
