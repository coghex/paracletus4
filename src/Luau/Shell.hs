-- | functions for the shell
module Luau.Shell where
import Prelude()
import UPrelude
import Control.Monad.IO.Class ( liftIO )
import Data.List.Split ( splitOn )
import Data ( Shell(..), ID(..) )
import Load.Data ( Tile(..), TilePos(..), TileTex(..), DrawState(..), DSStatus(..) )
import Luau.Data ( ShellCmd(..) )
import Sign.Log (LogT(..), MonadLog(..), log', sendCapture)
import Sign.Data (LoadResult(..), Capture(..))
import Vulk.Font ( indexTTFData, TTFData(..), GlyphMetrics(..) )
import qualified Vulk.GLFW as GLFW

-- | processing of shell commands
processShellCommand ∷ (MonadLog μ,MonadFail μ) ⇒ DrawState → ShellCmd → LogT μ LoadResult
processShellCommand ds ShToggle       = do
  -- sets the capture in the input thread
  let cap = if shLoaded (dsShell ds) then CaptureNULL else CaptureShell
  sendCapture cap
  return $ LoadResultDrawState
    $ ds { dsShell  = toggleShell (dsShell ds)
         , dsStatus = DSSReload }
processShellCommand ds (ShKey key mk) = do
  str ← liftIO $ GLFW.calcInpKey key mk
  return $ LoadResultDrawState
    $ ds { dsShell  = stringShell (dsShell ds) str
         , dsStatus = DSSReload }
processShellCommand _  cmd            = do
  return $ LoadResultError $ "unknown shell command: " ⧺ show cmd

-- | turns shell on and off
toggleShell ∷ Shell → Shell
toggleShell shell = shell { shLoaded = not (shLoaded shell) }

-- | sends string to shell
stringShell ∷ Shell → String → Shell
stringShell sh str = sh { shTabbed = Nothing
                        , shInpStr = newStr
                        , shCursor = (shCursor sh) + (length str) }
  where newStr = (take (shCursor sh) (shInpStr sh)) ⧺ str ⧺ (drop (shCursor sh) (shInpStr sh))

-- | a combination of every tile needed for the shell
shTiles ∷ Int → [TTFData] → Shell → [Tile]
shTiles fontsize ttfdata sh = tiles
  where tiles    = txttiles ⧺ boxtiles
        pos      = (-10,5)
        boxtiles = boxTiles fontsize pos sh
        txttiles = txtTiles fontsize ttfdata pos sh 256

-- | every tile needed for the text, fills the rest with empty buffer
txtTiles ∷ Int → [TTFData] → (Double,Double) → Shell → Int → [Tile]
txtTiles fontsize ttfdata pos sh buffSize = case shLoaded sh of
  False → take buffSize $ repeat
    (Tile IDNULL (TilePos (0,0) (1,1)) (TileTex (0,0) (1,1) fontsize))
  True  → tiles ⧺ take (buffSize - length tiles)
    (repeat (Tile IDNULL (TilePos (0,0) (1,1)) (TileTex (0,0) (1,1) fontsize)))
    where tiles  = genStringTiles fontsize ttfdata (fst pos') pos' string
          string = genShellStr sh
          pos'   = (fst pos + 1, snd pos - 1)

-- | generates the tiles for a singe string
genStringTiles ∷ Int → [TTFData] → Double → (Double,Double) → String → [Tile]
genStringTiles _        _       _  _     []         = []
genStringTiles fontsize ttfdata x0 (x,y) (' ':str)
  = genStringTiles fontsize ttfdata x0 (x+0.1,y) str
genStringTiles fontsize ttfdata x0 (_,y) ('\n':str)
  = genStringTiles fontsize ttfdata x0 (x0,y-1) str
genStringTiles fontsize ttfdata x0 (x,y) (ch:str)   = case indexTTFData ttfdata ch of
  Nothing → genStringTiles fontsize ttfdata x0 (x,y) str
  Just (TTFData _ chInd (GlyphMetrics chW chH chX chY chA))
    → tile : genStringTiles fontsize ttfdata x0 (x+(2*chA),y) str
      where tile = Tile IDNULL (TilePos (x',y') (w',h'))
                               (TileTex (0,0) (1,1) chInd)
            (x',y') = (realToFrac(x+(2*chX)+chW)
                      ,realToFrac(y+(2*chY)-chH-0.1))
            (w',h') = (realToFrac chW
                      ,realToFrac chH)

-- | the shells state as a string
genShellStr ∷ Shell → String
genShellStr sh
  | (height > 8) = shortret
  | otherwise    = retstring
  where prompt    = shPrompt sh
        strsout   = genShellOut (shOutStr sh) (shRet sh)
        strsin    = shInpStr sh
        height    = length $ filter (≡ '\n') retstring
        retstring = strsout ⧺ prompt ⧺ strsin
        shortret  = flattenWith '\n' $ drop (height - 8) (splitOn "\n" retstring)
        flattenWith ∷ Char → [String] → String
        flattenWith _  []         = ""
        flattenWith ch (str:strs) = str ⧺ [ch] ⧺ flattenWith ch strs
genShellOut ∷ String → String → String
genShellOut out ""  = out
genShellOut out ret = (init out) ⧺ "> " ⧺ ret ⧺ "\n"

-- | a list of tiles that makes a box
boxTiles ∷ Int → (Double,Double) → Shell → [Tile]
boxTiles fontsize pos sh  = tiles
    where pos        = (-10,5)
          width      = 8
          height     = 2
          width'     = 2.0 * fromIntegral (width+1)
          height'    = 2.0 * fromIntegral (height+1)
          postl      = pos
          postr      = ((fst pos) + width', snd pos)
          posbl      = (fst pos, (snd pos) - height')
          posbr      = ((fst pos) + width',(snd pos) - height')
          blanktile  = Tile IDNULL (TilePos (0,0) (1,1))
                                   (TileTex (0,0) (1,1) fontsize)
          filltile   = Tile IDNULL (TilePos pos (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-9))
          righttile  = Tile IDNULL (TilePos postr (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-8))
          toptile    = Tile IDNULL (TilePos postl (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-7))
          toprtile   = Tile IDNULL (TilePos postr (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-6))
          topltile   = Tile IDNULL (TilePos postl (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-5))
          bottile    = Tile IDNULL (TilePos posbl (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-4))
          botrtile   = Tile IDNULL (TilePos posbr (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-3))
          botltile   = Tile IDNULL (TilePos posbl (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-2))
          lefttile   = Tile IDNULL (TilePos postl (1,1))
                                   (TileTex (0,0) (1,1) (fontsize-1))
          toptiles   = tileHor width toptile
          bottiles   = tileHor width bottile
          lefttiles  = tileVer height lefttile
          righttiles = tileVer height righttile
          fill       = tileFill width height filltile
          tiles'     = topltile : toprtile : botltile : botrtile
                     : (toptiles ⧺ bottiles ⧺ lefttiles ⧺ righttiles ⧺ fill)
          tiles      = case shLoaded sh of
                         False → take (length tiles') $ repeat blanktile
                         True  → tiles'

tileHor ∷ Int → Tile → [Tile]
tileHor 0 tile = []
tileHor n (Tile id (TilePos (x,y) (w,h)) tex) = tile' : tileHor (n-1) tile'
  where tile' = Tile id (TilePos (x+2,y) (w,h)) tex
tileVer ∷ Int → Tile → [Tile]
tileVer 0 tile = []
tileVer n (Tile id (TilePos (x,y) (w,h)) tex) = tile' : tileVer (n-1) tile'
  where tile' = Tile id (TilePos (x,y-2) (w,h)) tex
tileFill ∷ Int → Int → Tile → [Tile]
tileFill 0 h _                                   = []
tileFill w h (Tile id (TilePos (x,y) sc) tex) = tiles ⧺ (tileFill (w-1) h tile')
  where tile' = Tile id (TilePos (x+2,y) sc) tex
        tiles = tileVer h tile'



