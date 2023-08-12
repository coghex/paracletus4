module Luau.Util where
-- we have some utility funcitons that work in lua monad
import Prelude()
import UPrelude
import qualified HsLua as Lua
import GHC.Stack ( HasCallStack )

-- | custom head so we can have errors
vhead ∷ HasCallStack ⇒ [α] → Lua.Lua (Maybe α)
vhead a = if length a > 0 then return $ Just $ head a
  else do
    Lua.liftIO $ print "LUAERROR: head on empty string"
    return Nothing
-- | custom tail so we can have errors
vtail ∷ HasCallStack ⇒ [α] → Lua.Lua [α]
vtail a = if length a > 0 then return $ tail a
  else do
    Lua.liftIO $ print "LUAERROR: tail on empty string"
    return []
