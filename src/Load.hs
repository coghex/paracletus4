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
import Data ( PrintArg(PrintNULL), Color(..) )
import Data.Aeson as A
import Data.Maybe ( fromMaybe )
import Data.List.Split ( splitOn )
import Data.Map as Map
import Data.String ( fromString )
import Load.Data
import Load.Util ( emptyTiles )
import Luau.Data ( Window(..), Page(..) )
import Prog.Data
import Prog.Buff ( generateDynData )
import Sign.Data
import Sign.Log
import Vulk.Calc ( calcVertices )
import Vulk.Data ( Verts(Verts) )
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
  tsNew ← readTimerBlocked
  runLoadLoop ds tsNew
runLoadLoop ds TStart = do
  start ← liftIO getCurrentTime
  timerState ← readTimer
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
  mcmd ← readCommand
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
            log' (LogDebug 1) "regenerating verts and dyns"
            let verts = Verts $ calcVertices $ emptyTiles $ length tiles
                tiles = dsTiles ds
                dyns  = generateDynData tiles
            sendLoadEvent verts dyns
            processCommands ds' { dsStatus = DSSNULL }
          DSSNULL   → processCommands ds'
        LoadResultError str     → do
          log' LogError $ "load command error: " ⧺ str
          return ds
        _                       → do
          log' LogError "unknown load command result"
          return ds
    Nothing → return ds


-- | this is the case statement for processing load commands
processCommand ∷ (MonadLog μ,MonadFail μ)
  ⇒ DrawState → LoadCmd → LogT μ LoadResult
processCommand ds cmd = case cmd of
  LoadState (LSCRegisterTextureMap fp) → do
    log' (LogDebug 1) "registering texture map..."
    TextureMap tm ← readTextureMap fp
    sendTextures tm
    return $ LoadResultDrawState $ ds { dsTexMap = TextureMap tm }
  LoadTest → do
    let tm = dsTexMap ds
    log' (LogDebug 1) $ "texMap: " ⧺ show tm
    return $ LoadResultSuccess
  LoadTile tilePos t → do
    let TextureMap tm = dsTexMap ds
        tile          = Tile tilePos tt
        tt            = findTex t tm
    return $ LoadResultDrawState ds { dsTiles  = tile : (dsTiles ds) }
  LoadReload → do
    return $ LoadResultDrawState ds { dsStatus = DSSReload }
  _ → return LoadResultSuccess

findTex ∷ String → [(String,Tex)] → TileTex
findTex n [] = TileTex (0,0) (1,1) 0
findTex n ((name,(Tex fp ind size)):texs)
  | n ≡ name  = TileTex (0,0) size ind
  | otherwise = findTex n texs

-- | reads the texture data file
readTextureMap ∷ (MonadLog μ, MonadFail μ)
 ⇒ String → LogT μ TextureMap
readTextureMap path = do
  inputSettingsFile ← liftIO $ openFile path ReadMode
  contents          ← liftIO $ hGetContents inputSettingsFile
  let textureMap = A.decode $ fromString contents
  case textureMap of
    Just (InTexJson k0) → do
      liftIO $ hClose inputSettingsFile
      let texMap = createTextureMap 0 k0
      return $ TextureMap texMap
    Nothing → do
      log' LogError $ "[Input] error decoding " ⧺ path
      liftIO $ hClose inputSettingsFile
      return $ TextureMap []

-- | turns the list into a map
createTextureMap ∷ Int → [TextureData] → [(String,Tex)]
createTextureMap _ []       = []
createTextureMap n ((TextureData name fp):tds)
  = texdat : createTextureMap (n+1) tds
    where texdat = (name, Tex fp n (1,1))

-- | initial draw state
initDrawState ∷ DrawState
initDrawState = DrawState DSSNULL (TextureMap []) [] []

