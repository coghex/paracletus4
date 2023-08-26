-- defines the basic game
require "mod/base/game"
require "mod/base/json"
require "mod/game/default/menu"
require "mod/game/default/load"
require "mod/game/default/input"
require "mod/game/default/texture"
-- this runs once at the beginning
function initMod ()
    -- basic UI elements
    game.logDebug("default mod: initMod()")
    initInput ()
    initTextures ()
    ls = initLoadScreen ()
    game.selectWin (ls)
    game.start()
    loaded = 0
end
function runMod ()
    if loaded < 1 then
        loaded = loaded + 1
    elseif loaded < 2 then
        m = initMenu()
        game.selectWin (m)
        loaded = loaded + 1
        game.logDebug("loaded")
    end
    game.logDebug("default mod: runMod()")
end
