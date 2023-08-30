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
    font = initTextures ()
    ls = initLoadScreen (font)
    game.selectWin (ls)
    game.start()
end
function runMod ()
    if loaded == 2 then
        m = initMenu(font)
        game.selectWin (m)
        game.logDebug("loaded")
        loaded = 3
    end
    game.logDebug("default mod: runMod()")
end
