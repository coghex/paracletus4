{-# LANGUAGE Strict #-}
-- | mouse related functionality, inluding scroll,
--   click, and positional monitoring 
module Prog.Mouse where
-- mouse input is handled
import Prelude()
import UPrelude
import Data.List.Split ( splitOn )
import Data.Bifunctor ( bimap )
import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO(..) )
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
  let newms  = (mouseSt is) { mousePos = pos }
      elems  = findElemsUnder pos' $ inputElems is
      pos'   = normalizePos pos siz
      elems' = pullOutButtons elems
  -- toggles button states
  buttst ← if length elems > 0 then do
    writeQueue'' env LoadQueue $ QCLoadCmd $ LoadInput
      $ LIToggleButtons elems' True
    return True
  else if buttSt is then do
    writeQueue'' env LoadQueue $ QCLoadCmd $ LoadInput LIClearButtons
    return False
  else return False
  -- sends the mouse position to the main thread to move the camera
  -- when the middle mouse button is pressed
  case mouse3 newms of
    Nothing    → return is { mouseSt = newms
                           , buttSt  = buttst }
    Just (x,y) → do
      let (mx,my)   = pos'
          (x',y')   = normalizePos (x,y) siz
          (x'',y'') = (64*(mx - x'), 64*(my - y'))
          newnewnewms  = newms { mouse3 = Just pos }
      writeQueue'' env EventQueue $ QCEvent $ EventSys $ SysMoveCam (x'',y'',0)
      return is { mouseSt = newnewnewms
                , buttSt  = buttst }

-- | takes a list of inputElems and gives back the buttons
pullOutButtons ∷ [InputElem] → [Button]
pullOutButtons []                  = []
pullOutButtons ((IEButt butt):ies) = butt : pullOutButtons ies
pullOutButtons (_:ies)             = pullOutButtons ies

-- | processes mouse button clicks
processMouseButton ∷ Env → InputState → GLFW.Window → GLFW.MouseButton
  → GLFW.MouseButtonState → GLFW.ModifierKeys → IO InputState
processMouseButton env is win mb mbs mk = do
  -- left mouse button
  if mb ≡ GLFW.mousebutt1 then do
    pos ← GLFW.getCursorPos win
    siz ← GLFW.getWindowSize win
    let elems = findElemsUnder pos' $ inputElems is
        pos'  = normalizePos pos siz
    sendClick env elems pos
    return is
  -- middle mouse button
  else if mb ≡ GLFW.mousebutt3 then do
    case mouse3 (mouseSt is) of
      Nothing → if mbs ≡ GLFW.MouseButtonState'Pressed then do
                  pos' ← liftIO $ GLFW.getCursorPos win
                  let pos   = bimap realToFrac realToFrac pos'
                  --    pos   = ((realToFrac (fst pos')),(realToFrac (snd pos')))
                      newIS = is { mouseSt = (mouseSt is) { mouse3 = Just pos } }
                  return newIS
                else return is
      Just _  → if mbs ≡ GLFW.MouseButtonState'Released then do
                  let newIS = is { mouseSt = (mouseSt is) { mouse3 = Nothing } }
                  return newIS
                else return is
  else return is

-- | sends data to the load thread corresponding
--   to the first input elem in the list
sendClick ∷ Env → [InputElem] → (Double,Double) → IO ()
sendClick env []                pos = do
  writeQueue'' env LoadQueue $ QCLoadCmd $ LoadInput $ LILeftClick pos
sendClick env ((IEButt butt):_) _   =
  writeQueue'' env LoadQueue $ QCLoadCmd $ LoadInput $ LIButton butt
sendClick env (_:es)            pos = sendClick env es pos

-- | returns the list of input elems at the specified position
findElemsUnder ∷ (Double,Double) → [InputElem] → [InputElem]
findElemsUnder _   []     = []
findElemsUnder pos (e:es)
  | elemUnder pos e = e : findElemsUnder pos es
  | otherwise       = findElemsUnder pos es

-- | returns true if the element is under the position
elemUnder ∷ (Double,Double) → InputElem → Bool
elemUnder (mx,my) (IEButt (Button _ _ (x,y) (w,h)))
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
