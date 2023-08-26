{-# LANGUAGE AllowAmbiguousTypes #-}
-- | lua interpreter runs initLuau function from
--   each mod once at the beginning the runLuau
--   once for each mod every lua thread tick.
module Luau where
-- lua loader and interpreter
import Prelude()
import UPrelude
import Data.List (sort)
import Data.Maybe ( fromMaybe )
import Data.String ( fromString )
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified Data.Map as Map
import qualified HsLua as Lua
import Control.Monad.Catch ( catch )
import System.Directory (getDirectoryContents)
import System.FilePath (combine)
import Luau.Command
import Luau.Data ( UserData(..) )
import Prog.Data ( Env(..), ChanName(..), QueueName(..), QueueCmd(..) )
import Sign.Data
    ( Event(EventLog, EventSys),
      LogLevel(..), LoadCmd(..),
      SysAction(SysRecreate),
      TState(..) )
import Sign.Thread (threadDelay)
import Sign.Queue (readChan, tryReadChan, writeQueue)
import Sign.Var (atomically)
import Sign.Util ( log', readChan', tryReadQueue', writeQueue'' )

-- | initialization of each mod file, as well as registering all
--   of the raw functions, and kickoff of the vertex generation
luauThread ∷ Env → IO ()
luauThread env = do
    modFiles ← findModFiles "mod/game/"
    if modFiles ≡ [] then do
      log' env (LogDebug 1) "[Luau] no mod files in mod/game/"
    else do
      log' env (LogDebug 1) "[Luau] initializing Luau with mods:"
      log' env (LogDebug 1) $ "[Luau]    " ⧺ modFiles
      let ls = envLuaSt env
      _ ← Lua.runWith ls $ do
        Lua.registerHaskellFunction (fromString "rawExit")      (hsExit         env)
        Lua.registerHaskellFunction (fromString "logDebug")     (hsLogDebug     env)
        Lua.registerHaskellFunction (fromString "logInfo")      (hsLogInfo      env)
        Lua.registerHaskellFunction (fromString "logError")     (hsLogError     env)
        Lua.registerHaskellFunction (fromString "newID")        (hsNewID        env)
        Lua.registerHaskellFunction (fromString "rawNewTile")   (hsNewTile      env)
        Lua.registerHaskellFunction (fromString "rawNewAtlas")  (hsNewAtlas     env)
        Lua.registerHaskellFunction (fromString "rawNewText")   (hsNewText      env)
        Lua.registerHaskellFunction (fromString "rawReload")    (hsReload       env)
        Lua.registerHaskellFunction (fromString "rawRecreate")  (hsRecreate     env)
        Lua.registerHaskellFunction (fromString "rawNewWindow") (hsNewWindow    env)
        Lua.registerHaskellFunction (fromString "rawSelectWin") (hsSelectWin    env)
        Lua.registerHaskellFunction (fromString "rawLoadFont")  (hsLoadFont     env)
        Lua.registerHaskellFunction (fromString "rawStart")     (hsStart        env)
        Lua.registerHaskellFunction
          (fromString "rawRegisterInputKeys")          (hsRegisterInputKeys  env)
        Lua.registerHaskellFunction
          (fromString "rawRegisterTileMap")            (hsRegisterTileMap    env)
        Lua.registerHaskellFunction
          (fromString "rawRegisterTextureMap")         (hsRegisterTextureMap env)
        Lua.openlibs
        _ ← Lua.dofile $ Just "mod/base/game.lua"
        Lua.invoke (fromString "initLuau") modFiles ∷ Lua.LuaE Lua.Exception Int
      luauLoop TPause env initUD modFiles

-- | the loop runs lua commands every loop and maintains a state
luauLoop ∷ TState → Env → UserData → String → IO ()
luauLoop TPause env ud modFiles = do
  tsNew ← readChan' env LuaChan
  case tsNew of
    Nothing → do
      threadDelay 1000
      luauLoop TPause env ud modFiles
      --log' env LogError "there is no lua chan"
    Just c0 → luauLoop c0 env ud modFiles
luauLoop TStart env ud modFiles = do
  start ← getCurrentTime
  tsMby ← readChan' env LuaChan
  let tsNew = fromMaybe TStart tsMby
      ls = envLuaSt env
  _ ← Lua.runWith ls $ do
    Lua.openlibs
    _ ← Lua.dofile $ Just "mod/base/game.lua"
    Lua.invoke (fromString "runLuau") modFiles ∷ Lua.LuaE Lua.Exception Int
  end ← getCurrentTime
  let diff  = diffUTCTime end start
      usecs = floor (toRational diff * 1000000) ∷ Int
      delay = n*1000 - usecs
      n     = 1000
  if delay > 0 then threadDelay delay else return ()
  luauLoop tsNew env ud modFiles
luauLoop TStop  _   _  _        = return ()
luauLoop TNULL  _   _  _        = return ()

-- initializes empty user data
initUD ∷ UserData
initUD = UserData Map.empty

-- | simple utility function that may or may not work on windows
findModFiles ∷ String → IO String
findModFiles path = do
  paths ← getDirectoryContents "mod/game/"
  return $ collapsePaths $ map (combine path)
           $ sort $ filter filterOutPathJunk paths
  where filterOutPathJunk ∷ FilePath → Bool
        filterOutPathJunk "."  = False
        filterOutPathJunk ".." = False
        filterOutPathJunk x    = stripname == ".lua"
          where stripname = drop ((length x) - 4) x
        collapsePaths ∷ [String] → String
        collapsePaths [] = ""
        collapsePaths [str]      = str
        collapsePaths (str:strs) = str ⧺ ";" ⧺ collapsePaths strs
