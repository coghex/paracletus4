module Luau.ShCmd where
-- some auxillary shell lua functions
-- loaded in by default when shell is run
import Prog.Data
import Sign.Data
import Sign.Queue
import Sign.Var
import Vulk.Data
import Luau.Data
import Sign.Util ( writeQueue'' )
import qualified HsLua as Lua
import Data.String ( fromString )

loadShCmds ∷ Env → IO ()
loadShCmds env = do
  let ls = envLuaSt env
  _ ← Lua.runWith ls $ do
    Lua.registerHaskellFunction (fromString "echo")    (hsEcho    env)
    Lua.registerHaskellFunction (fromString "history") (hsHistory env)
    Lua.registerHaskellFunction (fromString "clear")   (hsClear   env)
    Lua.registerHaskellFunction (fromString "exit")    (hsExit    env)
    -- need this or haskell forgets what an exception is
      ∷ Lua.LuaE Lua.Exception ()
  return ()

hsExit ∷ Env → Lua.Lua ()
hsExit env = Lua.liftIO $ writeQueue'' env EventQueue $ QCEvent $ EventSys SysExit

hsEcho ∷ Env → String → Lua.Lua ()
hsEcho env str = Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadShell $ ShEcho str

hsHistory ∷ Env → Lua.Lua ()
hsHistory env = Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadShell ShHistory

hsClear ∷ Env → Lua.Lua ()
hsClear env = Lua.liftIO $ writeQueue'' env LoadQueue $ QCLoadCmd $ LoadShell ShClear
