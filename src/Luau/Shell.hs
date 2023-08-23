-- | functions for the shell
module Luau.Shell where
import Prelude()
import UPrelude
import Control.Monad.IO.Class ( liftIO )
import Data.List.Split ( splitOn )
import Data ( Shell(..), ShellCard(..), ID(..) )
import Prog.Data ( Env(..) )
import Load.Data ( Tile(..), TilePos(..), TileTex(..), DrawState(..), DSStatus(..) )
import Luau.Data ( ShellCmd(..) )
import Luau.ShCmd ( loadShCmds )
import Sign.Log (LogT(..), MonadLog(..), log', sendCapture, sendTimerState, askLog, Log(..))
import Sign.Data (LoadResult(..), Capture(..), LogLevel(..), TState(..), TimerName(..))
import Vulk.Font ( indexTTFData, TTFData(..), GlyphMetrics(..), indexTTF )
import qualified Vulk.GLFW as GLFW
import qualified HsLua as Lua
import qualified Data.ByteString.Char8 as BL

-- | processing of shell commands
processShellCommand ∷ (MonadLog μ,MonadFail μ) ⇒ DrawState → ShellCmd → LogT μ LoadResult
processShellCommand ds ShToggle       = do
  -- sets the capture in the input thread
  let cap = if shLoaded (dsShell ds) then CaptureNULL else CaptureShell
  let tst = if shLoaded (dsShell ds) then TPause else TStart
  sendCapture cap
  sendTimerState ShellCursorTimer tst
  return $ LoadResultDrawState
    $ ds { dsShell  = toggleShell (dsShell ds)
         , dsStatus = DSSReload }
processShellCommand ds (ShKey key mk)
  | GLFW.modifierKeysControl mk
  = case key of
      GLFW.Key'C → return $ LoadResultDrawState $ ds { dsShell  = sh'
                                                     , dsStatus = DSSReload }
                where sh' = sh { shTabbed = Nothing
                               , shCursor = 0
                               , shInpStr = ""
                               , shCache  = ""
                               , shHistI  = -1
                               , shOutStr = retstring }
                      retstring = shOutStr sh ⧺ shPrompt sh ⧺ shInpStr sh ⧺ "\n"
                      sh = dsShell ds
      GLFW.Key'A → return $ LoadResultDrawState
                     $ ds { dsShell  = (dsShell ds) { shCursor = 0 }
                          , dsStatus = DSSReload }
      GLFW.Key'E → return $ LoadResultDrawState
                     $ ds { dsShell = (dsShell ds)
                            { shCursor = length (shInpStr (dsShell ds)) }
                          , dsStatus = DSSReload }
      _     → return LoadResultSuccess
  | key ≡ GLFW.Key'Backspace
  = return $ LoadResultDrawState
      $ ds { dsShell  = delShell (dsShell ds)
           , dsStatus = DSSReload }
  | key ≡ GLFW.Key'Enter
  = do (Log _   env _   _   _) ← askLog
       newSh ← liftIO $ evalShell env $ dsShell ds
       return $ LoadResultDrawState
         $ ds { dsShell  = newSh }
  | key ≡ GLFW.Key'Up
  = return $ LoadResultDrawState
      $ ds { dsShell  = upShell (dsShell ds)
           , dsStatus = DSSReload }
  | key ≡ GLFW.Key'Down
  = return $ LoadResultDrawState
      $ ds { dsShell  = downShell (dsShell ds)
           , dsStatus = DSSReload }
  | key ≡ GLFW.Key'Right
  = return $ LoadResultDrawState
      $ ds { dsShell  = cursorShell 1 (dsShell ds)
           , dsStatus = DSSReload }
  | key ≡ GLFW.Key'Left
  = return $ LoadResultDrawState
      $ ds { dsShell  = cursorShell (-1) (dsShell ds)
           , dsStatus = DSSReload }
  | otherwise
  = do str ← liftIO $ GLFW.calcInpKey key mk
       return $ LoadResultDrawState
         $ ds { dsShell  = stringShell (dsShell ds) str
              , dsStatus = DSSReload }
processShellCommand ds (ShEcho str)
  = return $ LoadResultDrawState
      $ ds { dsShell  = (dsShell ds) { shRet = (shRet (dsShell ds)) ⧺ str }
           , dsStatus = DSSReload }
processShellCommand _  cmd            = do
  return $ LoadResultError $ "unknown shell command: " ⧺ show cmd

-- sends directional key to shell
directionShell ∷ ShellCard → Shell → Shell
directionShell ShellUp    sh = upShell   sh
directionShell ShellDown  sh = downShell sh
directionShell ShellLeft  sh = cursorShell (-1) sh
directionShell ShellRight sh = cursorShell 1    sh

-- cycles through shell history
upShell ∷ Shell → Shell
upShell sh
  | shHist sh ≡ [] = sh
  | otherwise      = sh { shInpStr = shinpstr
                        , shHistI  = incShHist
                        , shCursor = length shinpstr }
  where incShHist = if shHistI sh ≥ length (shHist sh) then 0 else shHistI sh + 1
        shinpstr  = shHist sh !! (incShHist `mod` length (shHist sh))
downShell ∷ Shell → Shell
downShell sh
  | shHist sh ≡ [] = sh
  | shHistI sh ≥ 0 = sh { shInpStr = shinpstr
                        , shHistI  = max (-1) (shHistI sh - 1)
                        , shCursor = length shinpstr }
  | otherwise      = sh { shInpStr = "" }
  where shinpstr = shHist sh !! (shHistI sh `mod` length (shHist sh))
-- move shell cursor
cursorShell ∷ Int → Shell → Shell
cursorShell n sh = sh { shCursor = n' }
  where n' = max 0 $ min (length (shInpStr sh)) $ (shCursor sh) + n


-- | turns shell on and off
toggleShell ∷ Shell → Shell
toggleShell shell = shell { shLoaded = not (shLoaded shell) }

-- | sends string to shell
stringShell ∷ Shell → String → Shell
stringShell sh str = sh { shTabbed = Nothing
                        , shInpStr = newStr
                        , shCursSt = True
                        , shCursor = shCursor sh + length str }
  where newStr = take (shCursor sh) (shInpStr sh) ⧺ str ⧺ drop (shCursor sh) (shInpStr sh)

-- | deletes a character from the shell
delShell ∷ Shell → Shell
delShell sh = sh { shInpStr = newStr
                 , shCursor = max 0 (shCursor sh - 1) }
  where newStr = initS (take (shCursor sh) (shInpStr sh)) ⧺ drop (shCursor sh) (shInpStr sh)
        initS ""  = ""
        initS str = init str

-- evaluates lua commands in IO
evalShell ∷ Env → Shell → IO Shell
evalShell env shell = do
  let ls = envLuaSt env
  if shLibs shell then
    return ()
  else do
    loadShCmds env
  (ret,outbuff) ← execShell ls (shInpStr shell)
  let retstring = if length (shOutStr shell) ≡ 0
        then case outbuff of
          "nil" → shOutStr shell ⧺ shPrompt shell ⧺ shInpStr shell
                  ⧺ "\n" ⧺ show ret ⧺ "\n"
          _     → shOutStr shell ⧺ shPrompt shell ⧺ shInpStr shell
                  ⧺ "\n" ⧺ show ret ⧺ " > " ⧺ outbuff ⧺ "\n"
        else case outbuff of
          "nil" → case shRet shell of
                    "" → init (shOutStr shell) ⧺ "\n"
                         ⧺ shPrompt shell ⧺ shInpStr shell ⧺ "\n" ⧺ show ret ⧺ "\n"
                    _  → init (shOutStr shell) ⧺ "> " ⧺ shRet shell
                         ⧺ "\n" ⧺ shPrompt shell ⧺ shInpStr shell
                         ⧺ "\n" ⧺ show ret ⧺ "\n"
          _     → case shRet shell of
                    "" → init (shOutStr shell)
                         ⧺ "\n" ⧺ shPrompt shell ⧺ shInpStr shell
                         ⧺ "\n" ⧺ show ret ⧺ " > " ⧺ outbuff ⧺ "\n"
                    _  → init (shOutStr shell) ⧺ "> " ⧺ shRet shell
                         ⧺ "\n" ⧺ shPrompt shell ⧺ shInpStr shell ⧺ "\n"
                         ⧺ show ret ⧺ " > " ⧺ outbuff ⧺ "\n"
      shell' = shell { shInpStr = ""
                     , shOutStr = retstring
                     , shTabbed = Nothing
                     , shRet    = ""
                     , shLoaded = True
                     , shHistI  = -1
                     , shHist   = [shInpStr shell] ⧺ shHist shell
                     , shCursSt = True
                     , shCursor = 0 }
  return shell'
execShell ∷ Lua.State → String → IO (Lua.Status,String)
execShell ls ""  = return (Lua.OK,"")
execShell ls str = do
  let str' = case last str of
               ')' → str
               _   → str ⧺ "()"
  luaerror ← Lua.runWith ls $ Lua.loadstring $ BL.pack str'
  _   ← Lua.runWith ls $ Lua.pcall 0 1 Nothing
  ret ← Lua.runWith ls $ (Lua.tostring' $ Lua.nthBottom (-1)
    ∷ Lua.LuaE Lua.Exception BL.ByteString)
  Lua.runWith ls $ Lua.pop $ fromEnum $ Lua.nthBottom (-1)
  return $ (luaerror,(BL.unpack ret))

-- | a combination of every tile needed for the shell
shTiles ∷ Int → [TTFData] → Shell → [Tile]
shTiles fontsize ttfdata sh = tiles
  where tiles     = curstiles ⧺ txttiles ⧺ boxtiles
        pos       = (-10,5)
        boxtiles  = boxTiles fontsize pos sh
        txttiles  = txtTiles fontsize ttfdata pos sh 256
        curstiles = cursTiles fontsize ttfdata pos sh

-- | the cursor tiles
cursTiles ∷ Int → [TTFData] → (Double,Double) → Shell → [Tile]
cursTiles fontsize ttfdata pos sh = case shLoaded sh of
  False → [Tile IDNULL (TilePos (0,0) (1,1)) (TileTex (0,0) (1,1) fontsize)]
  True  → case indexTTFData ttfdata '|' of
    Nothing → [Tile IDNULL (TilePos (0,0) (1,1)) (TileTex (0,0) (1,1) fontsize)]
    Just (TTFData _ _ (GlyphMetrics chW chH _ _ _)) → if shCursSt sh then
        [Tile IDNULL (TilePos pos' size)  (TileTex (0,0) (1,1) 93)]
      else [Tile IDNULL (TilePos (0,0) (1,1)) (TileTex (0,0) (1,1) fontsize)]
          where size  = (realToFrac chW, 2*realToFrac chH)
                pos'  = ((fst pos) + xcurs, (snd pos) - ycurs - 0.1)
                xcurs = findCursPos ttfdata $ take n $ shInpStr sh
                ycurs = fromIntegral $ min 9 $ length $ splitOn "\n" $ shOutStr sh
                n     = shCursor sh

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

-- | returns the x position of the shell's cursor
findCursPos ∷ [TTFData] → String → Double
findCursPos _       []        = 1.8
findCursPos ttfdata (' ':str) = 0.1 + findCursPos ttfdata str
findCursPos ttfdata (ch:str)  = case indexTTFData ttfdata ch of
  Nothing → findCursPos ttfdata str
  Just (TTFData _ _ (GlyphMetrics _ _ _ _ chA)) → wid + findCursPos ttfdata str
    where wid = 2*chA

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
          height     = 6
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



