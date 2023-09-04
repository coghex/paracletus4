{-# LANGUAGE Strict #-}
-- | a huge portion of the engine is here in the load thread.
--   any time a change needs to occur on screen, the abstract
--   representation of the draw state needs to be calculated
--   into dynamic data and passed to the main draw thread. any
--   changes to the number of objects on screen will trigger
--   this thread to also generate the verticies and indicies
module Load where
-- a thread to help recreate the swapchain
import Prelude ()
import UPrelude
import Data ( PrintArg(PrintNULL), Color(..), ID(..), Shell(..) )
import Data.Aeson as A
import Data.Maybe ( fromMaybe )
import Data.List.Split ( splitOn )
import qualified Data.Map as Map
import Data.String ( fromString )
import Game.Data ( World(..) )
import Game.World ( newWorld, generateWorldData, generateWorldTiles )
import Load.Data
import Load.Util
import Luau.Shell ( toggleShell, shTiles, processShellCommand, positionShell, genShellStr )
import Prog.Data
import Prog.Buff ( generateDynData )
import Prog.Mouse ( calcTextBoxSize )
import Sign.Data
import Sign.Log
import Time ( processTimer )
import Vulk.Calc ( calcVertices )
import Vulk.Data ( Verts(Verts) )
import Vulk.Font ( TTFData(..), Font(..), findFont, calcFontOffset, findFontData )
import Util ( newID, blackColor, greenColor )
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class ( liftIO, MonadIO(..) )
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Vulk.GLFW as GLFW
import System.Log.FastLogger (LogType'(LogStdout))
import System.IO ( openFile, hGetContents, hClose, IOMode(..) )

-- | threaded loop provides work so main thread doesnt stutter
loadThread ∷ Env → IO ()
loadThread env = do
  logger ← makeDefaultLogger env (LogStdout 4096) (LogDebug 1)
  runLog logger $ runLoadLoop initDS TStop
  where initDS = initDrawState
runLoadLoop ∷ (MonadLog μ,MonadFail μ) ⇒ DrawState → TState → LogT μ ()
runLoadLoop ds TStop = do
  -- loop starts almost immediately
  tsNew ← readLoadTimerBlocked
  runLoadLoop ds tsNew
runLoadLoop ds TStart = do
  start ← liftIO getCurrentTime
  timerState ← readLoadTimer
  tsNew ← case timerState of
    Nothing → return TStart
    Just x  → return x
  ds' ← processCommands ds
  end ← liftIO getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = 1000 - usecs
  if delay > 0
    then liftIO $ threadDelay delay
    else return ()
  runLoadLoop ds' tsNew
-- pause not needed for this timer
runLoadLoop _ TPause = return ()
runLoadLoop _ TNULL  = return ()

-- | command queue processed once per loop
processCommands ∷ (MonadLog μ,MonadFail μ) ⇒ DrawState → LogT μ DrawState
processCommands ds = do
  mcmd ← readLoadCommand
  case mcmd of
    Just cmd → do
      ret ← processCommand ds cmd
      case ret of
        -- if command success keep processing commands
        LoadResultSuccess       → processCommands ds
        LoadResultDrawState ds' → case dsStatus ds' of
          DSSExit   → do
            sendSys SysExit
            return ds'
          -- sends the verts and dyns to the main thread
          DSSReload → do
            fontsize ← readFontSize
            ttfdata ← readFontMapM
            fonts ← readFonts
            olddyns ← readTVar DynsTVar
            shdat ← positionShell (dsWindow ds') (dsShell ds')
            let tiles = shell ⧺ wins
                wins  = if length fonts > 0 then
                              findTiles (dsLoad ds) fontsize (dsTexMap ds)
                                        fonts ttfdata (dsCurr ds') (dsWins ds')
                        else []
                dyns  = generateDynData tiles
                shell = shTiles fontsize (head ttfdata) shdat
            modifyTVar DynsTVar $ TVDyns dyns
            --log' (LogDebug 1) $ "[Load] ***regenerating dyns: "
            --                  ⧺ show (length tiles)
            if length olddyns ≠ length dyns then do
              let verts = Verts $ calcVertices $ reverse tiles
              modifyTVar VertsTVar $ TVVerts verts
              processCommands ds' { dsStatus = DSSNULL }
            else processCommands ds' { dsStatus = DSSNULL }
          DSSRecreate → do
            fontsize ← readFontSize
            ttfdata ← readFontMapM
            fonts ← readFonts
            shdat ← positionShell (dsWindow ds') (dsShell ds')
            -- TODO: find why we need to reverse this
            let verts = Verts $ calcVertices $ reverse tiles
                tiles = shell ⧺ wins
                wins  = if length fonts > 0 then
                              findTiles (dsLoad ds) fontsize (dsTexMap ds)
                                        fonts ttfdata (dsCurr ds') (dsWins ds')
                        else []
                dyns  = generateDynData tiles
                shell = shTiles fontsize (head ttfdata) shdat
            modifyTVar VertsTVar $ TVVerts verts
            modifyTVar DynsTVar $ TVDyns dyns
            sendSys SysRecreate
            log' (LogDebug 1) $ "[Load] ***regenerating verts and dyns: "
                              ⧺ show (length tiles)
            processCommands ds' { dsStatus = DSSNULL }
          DSSNULL   → processCommands ds'
        LoadResultError str     → do
          log' LogError $ "load command error: " ⧺ str
          return ds
        _                       → do
          log' LogError "unknown load command result"
          return ds
    Nothing → return ds

-- | returns the tiles in the current window
findTiles ∷ LoadState → Int → TextureMap → [Font] → [[TTFData]] → ID → Map.Map ID Window → [Tile]
findTiles loaded fontsize texmap fonts ttfdata win wins = case Map.lookup win wins of
                          Nothing → []
                          Just w0 → generateWinTiles fontsize texmap fonts
                                                     ttfdata (winElems w0)

 -- | loaded ≡ Loading = genStringTiles fontsize fd (fst pos) pos (2,2) "loading..."
 -- | loaded ≡ Loaded  = case Map.lookup win wins of
 --                          Nothing → []
 --                          Just w0 → generateWinTiles fontsize texmap fonts
 --                                                     ttfdata (winElems w0)
 -- | otherwise        = []
 --       where pos   = (0,0)
 --             fd    = ttfdata !! fontIndex font0
 --             font0 = head fonts
-- | returns the tiles in a list of elements
generateWinTiles ∷ Int → TextureMap → [Font] → [[TTFData]] → [WinElem] → [Tile]
generateWinTiles _        _      _     _       []       = []
generateWinTiles fontsize texmap fonts ttfdata (we:wes)
  = tiles ⧺ generateWinTiles fontsize texmap fonts ttfdata wes
  where tiles = generateElemTiles fontsize texmap fonts ttfdata we
generateElemTiles ∷ Int → TextureMap → [Font] → [[TTFData]] → WinElem → [Tile]
generateElemTiles fontsize texmap _     _       (WinElemTile tile) = [tile']
  where Tile id0 tp (TileTex tind tsiz t c) = tile
        tile' = Tile id0 tp (TileTex tind tsiz (t + fontsize) c)
generateElemTiles _        texmap fonts ttfdata (WinElemText text)
  = case findFont fonts id0 of
    Nothing → []
    Just font0 → genStringTiles fontsize' fd (fst pos) pos siz $ textString text
                   where fd = ttfdata !! fontIndex font0
                         fontsize' = calcFontOffset fonts $ fontIndex font0
    where pos = textPos text
          id0 = textFont text 
          siz = textSize text
generateElemTiles fontsize texmap fonts ttfdata (WinElemButton text buttid _ v)
  = case findFont fonts id0 of
    Nothing → []
    Just font0 → genStringTiles fontsize' fd (fst pos)
                                pos siz (textString text)
                                ⧺ backTiles
                   where fd        = ttfdata !! fontIndex font0
                         fontsize' = calcFontOffset fonts $ fontIndex font0
                         backTiles = case v of
                                       BSSelected → boxtiles
                                       BSNULL     → emptyTiles (length boxtiles)
                                                               fontsize
                        -- TODO: unhardcode the 4
                         boxtiles  = boxTiles fontsize pos' (0.25,0.25) (4,1) greenColor
    where pos = textPos text
          id0 = textFont text 
          siz = textSize text
          pos' = (fst pos, snd pos + 0.5)
generateElemTiles fontsize texmap fonts ttfdate (WinElemWorld world) =
  generateWorldTiles fontsize world texmap
generateElemTiles _        texmap _     _       WinElemNULL        = []

-- | this is the case statement for processing load commands
processCommand ∷ (MonadLog μ,MonadFail μ)
  ⇒ DrawState → LoadCmd → LogT μ LoadResult
processCommand ds cmd = case cmd of
  LoadState (LSCRegisterTextureMap fp) → do
    log' (LogDebug 1) "[Load] registering texture map..."
    TextureMap tm ← readTextureMap fp
    sendTextures tm
    return $ LoadResultDrawState $ ds { dsTexMap = TextureMap tm }
  LoadState (LSCSelectWin name) → do
    log' (LogDebug 1) $ "[Load] selecting win: " ⧺ show name
    -- test to make sure window exists and not already selected
    let wins = dsWins ds
        curr = dsCurr ds
    case Map.lookup name wins of
      Just _ → if curr ≡ name then do
          log' LogInfo $ "[Load] window " ⧺ show name ⧺ " already selected"
          return LoadResultSuccess
        else
          return $ LoadResultDrawState $ ds { dsCurr = name
                                            , dsStatus = DSSRecreate }
      Nothing → do
        log' LogWarn $ "[Load] window " ⧺ show name ⧺ " doesnt exist in :" ⧺ show wins
        return LoadResultSuccess
  LoadState (LSCSetGLFWWindow win) → do
    return $ LoadResultDrawState ds { dsWindow = Just win }
  LoadTest → do
        sendTest
        return $ LoadResultDrawState $ ds { dsStatus = DSSRecreate
                                          , dsLoad   = Loaded }
  LoadID → do
    log' LogInfo "[Load] creating id..."
    ID id0 ← liftIO newID
    writeIDTVar $ ID id0
    return LoadResultSuccess
  LoadNew lc → newChunk ds lc
  LoadGet gc → getData ds gc
  LoadShell shcmd → processShellCommand ds shcmd
  LoadInput input → processLoadInput ds input
  LoadTimer timer → processTimer ds timer
  LoadGen win → do
    wd ← generateWindowData ds win
    return $ LoadResultDrawState wd
  LoadReload → do
    return $ LoadResultDrawState ds { dsStatus = DSSReload }
  LoadRecreate → do
    return $ LoadResultDrawState ds { dsStatus = DSSRecreate }
  _ → return LoadResultSuccess

processLoadInput ∷ (MonadLog μ,MonadFail μ)
  ⇒ DrawState → LoadInputCmd → LogT μ LoadResult
processLoadInput ds (LIButton (Button func _ _ _)) = processLoadInputButtFunc ds func
processLoadInput ds (LIToggleButtons butts val)  = do
  fontsize ← readFontSize
  let ds' = toggleButtons ds fontsize butts val
  return $ LoadResultDrawState ds'
processLoadInput ds LIClearButtons  = do
  return $ LoadResultDrawState ds'
    where ds' = ds { dsWins   = clearButtons (dsWins ds)
                   , dsStatus = DSSReload }
processLoadInput _  input           = do
  return $ LoadResultError $ "[Load] unknown load input command " ⧺ show input
processLoadInputButtFunc ∷ (MonadLog μ,MonadFail μ)
  ⇒ DrawState → ButtonFunc → LogT μ LoadResult
processLoadInputButtFunc ds (BFLink link) = do
    -- test to make sure window exists and not already selected
    let wins = dsWins ds
        curr = dsCurr ds
    case Map.lookup link wins of
      Just _ → if curr ≡ link then do
          log' LogInfo $ "[Load] window " ⧺ show link ⧺ " already selected"
          return LoadResultSuccess
        else do
          sendGenerateWindowData link
          return $ LoadResultDrawState $ ds { dsCurr   = link
                                            , dsStatus = DSSRecreate }
      Nothing → return $ LoadResultError $ "no window " ⧺ show link
processLoadInputButtFunc ds bf            = do
  return $ LoadResultError $ "[Load] unknown button function " ⧺ show bf

-- | generates any data a window might need when switched to
sendGenerateWindowData ∷ (MonadLog μ,MonadFail μ) ⇒ ID → LogT μ ()
sendGenerateWindowData win = sendLoadCmd $ LoadGen win
generateWindowData ∷ (MonadLog μ,MonadFail μ) ⇒ DrawState → ID → LogT μ DrawState
generateWindowData ds win = do
  let newWins = Map.adjust (generateWindowDataInWin) win (dsWins ds)
  return ds { dsWins = newWins }
generateWindowDataInWin ∷ Window → Window
generateWindowDataInWin win = win { winElems = map generateWinElemData (winElems win) }
generateWinElemData ∷ WinElem → WinElem
generateWinElemData (WinElemWorld (World Nothing))
  = WinElemWorld $ World $ Just $ generateWorldData (2,2)
generateWinElemData we = we

-- | turns all buttons off
clearButtons ∷ Map.Map ID Window → Map.Map ID Window
clearButtons = Map.map clearButtonsInWins
clearButtonsInWins ∷ Window → Window
clearButtonsInWins win = win { winElems = clearButtonsInElems (winElems win) }
clearButtonsInElems ∷ [WinElem] → [WinElem]
clearButtonsInElems = map clearButtonsInElem
clearButtonsInElem ∷ WinElem → WinElem
clearButtonsInElem (WinElemButton txt id0 bf _) = WinElemButton txt id0 bf BSNULL
clearButtonsInElem we = we

-- | sets the designated buttons to the selected state
toggleButtons ∷ DrawState → Int → [Button] → Bool → DrawState
toggleButtons ds offset []           _   = ds
toggleButtons ds offset (butt:butts) val = toggleButtons ds' offset butts val
  where ds' = ds { dsWins   = Map.map (toggleButton offset butt val) (dsWins ds)
                 , dsStatus = DSSReload }
toggleButton ∷ Int → Button → Bool → Window → Window
toggleButton offset butt val win = win
  { winElems = toggleButtonInElems offset butt val (winElems win) }
toggleButtonInElems ∷ Int → Button → Bool → [WinElem] → [WinElem]
toggleButtonInElems offset butt val []     = []
toggleButtonInElems offset butt val (e:es) = e' : toggleButtonInElems offset butt val es
  where e' = toggleButtonInElem offset butt val e
toggleButtonInElem ∷ Int → Button → Bool → WinElem → WinElem
toggleButtonInElem offset (Button _ buttid _ _) val (WinElemButton text id0 buttfunc bs)
  | buttid ≡ id0 = WinElemButton text id0 buttfunc
                     $ if val then BSSelected else BSNULL
  | otherwise    = WinElemButton text id0 buttfunc BSNULL
toggleButtonInElem _      _                     _   we = we

--  where Button _ id0 pos size = butt
--        newtile               = Tile id0 (TilePos (0,0) (1,1))
--                                         (TileTex (0,0) (1,1) texInd)
--        texInd                = 0

-- | returns requested data on the user data TVar,
--   not all data is in the draw state so we have
--   to ask the main thread for some of it
getData ∷ (MonadLog μ,MonadFail μ) ⇒ DrawState → GetCommand → LogT μ LoadResult
getData _  GCWindow = do
  sendGetCommand GCWindow
  return LoadResultSuccess

-- | adds a chunk of data to the drawstate
newChunk ∷ (MonadLog μ,MonadFail μ) ⇒ DrawState → LoadChunk → LogT μ LoadResult
newChunk ds (LCWindow name)            = do
  ID id0 ← liftIO newID
  writeIDTVar $ ID id0
  return $ LoadResultDrawState $ ds { dsWins = Map.insert (ID id0) (Window name []) (dsWins ds) }
newChunk ds (LCTile  win pos tex)      = do
  log' (LogDebug 1) $ "[Load] loading new tile to win " ⧺ show win
  id0 ← liftIO newID
  let TextureMap tm = dsTexMap ds
      tile          = Tile id0 pos tt
      tt            = findTex tex tm
  writeIDTVar id0
  return $ LoadResultDrawState $ newTile ds win tile
newChunk ds (LCAtlas win pos tex tind) = do
  id0 ← liftIO newID
  let TextureMap tm = dsTexMap ds
      tile          = Tile id0 pos tt
      tt            = findAtlas 0 tind tex tm
  writeIDTVar id0
  return $ LoadResultDrawState $ newTile ds win tile
newChunk ds (LCText win text) = do
  id0 ← liftIO newID
  --let tile = Tile id0 (TilePos pos siz) $ TileTex (0,0) (1,1) (-10)
  writeIDTVar id0
  return $ LoadResultDrawState $ newText ds win text id0
newChunk ds (LCButton win (Text id (x,y) (w,h) font tx) buttfunc) = do
  id0 ← liftIO newID
  writeIDTVar id0
  fonts ← readFonts
  ttfdata ← readFontMapM
  let fontdata = findFontData fonts font ttfdata
      buttsize = calcTextBoxSize tx fontdata
  -- the button size is dummy data, we let the input thread sort it out
  sendInputElem $ IEButt $ Button buttfunc id0 (x,y) buttsize
  return $ LoadResultDrawState $ newButton ds win (Text id (x,y) (w,h) font tx)
                                           id0 buttfunc
newChunk ds (LCWorld win) = do
  return $ LoadResultDrawState $ newWorld win ds
newChunk _  LCNULL                     = return LoadResultSuccess
newChunk _  lc                         = do
  log' LogWarn $ "[Load] unknown load chunk command " ⧺ show lc
  return LoadResultSuccess

-- | adds a new tile to a window
newTile ∷ DrawState → ID → Tile → DrawState
newTile ds win tile =
  ds { dsWins = addElemToWin (dsWins ds) win (WinElemTile tile) }
-- | adds a new text section to a window
newText ∷ DrawState → ID → Text → ID → DrawState
newText ds win text id0 =
  ds { dsWins = addElemToWin (dsWins ds) win (WinElemText text') }
  where text' = text { textID = id0 }
-- | adds a button to a window
newButton ∷ DrawState → ID → Text → ID → ButtonFunc → DrawState
newButton ds win text id0 buttfunc =
  ds { dsWins = addElemToWin (dsWins ds) win (WinElemButton text' id0 buttfunc BSNULL) }
  where text' = text { textID = id0 }
-- | reads the texture data file
readTextureMap ∷ (MonadLog μ, MonadFail μ)
 ⇒ String → LogT μ TextureMap
readTextureMap path = do
  inputSettingsFile ← liftIO $ openFile path ReadMode
  contents          ← liftIO $ hGetContents inputSettingsFile
  let textureMap = A.decode $ fromString contents
  case textureMap of
    Just (InTexJson k0 k1) → do
      liftIO $ hClose inputSettingsFile
      let texMap = createTextureMap 0 k0 k1
      return $ TextureMap texMap
    Nothing → do
      log' LogError $ "[Input] error decoding " ⧺ path
      liftIO $ hClose inputSettingsFile
      return $ TextureMap []

-- | turns the list into a map
createTextureMap ∷ Int → [TextureData] → [AtlasData] → [(String,Tex)]
createTextureMap n []                          atl = createTextureAtlasMap n atl
createTextureMap n ((TextureData name fp):tds) atl
  = texdat : createTextureMap (n+1) tds atl
    where texdat = (name, Tex fp n (1,1))

-- | turns the list into a map
createTextureAtlasMap ∷ Int → [AtlasData] → [(String,Tex)]
createTextureAtlasMap _ []       = []
createTextureAtlasMap n ((AtlasData name fp w h):tds)
  = texdat : createTextureAtlasMap (n+1) tds
    where texdat = (name, Tex fp n (w,h))

-- | initial draw state
initDrawState ∷ DrawState
initDrawState = DrawState DSSNULL Nothing (TextureMap [])
                          Map.empty IDNULL initShell Loading
-- | creates a shell with empty values
initShell ∷ Shell
initShell = Shell "$> " Nothing 1 True "" "" "" ""
                  False False (0,0) (0,0) (-1) []
