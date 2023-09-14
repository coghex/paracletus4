-- defines the basic game
require "mod/base/game"
require "mod/base/json"
require "mod/game/default/menu"
require "mod/game/default/input"
require "mod/game/default/texture"
-- this runs once at the beginning
function initMod ()
    -- basic UI elements
    game.logDebug("default mod: initMod()")
    game.setDebug("fps")
    game.setDebug("grid")
    initInput ()
    fonts = initTextures ()
    m = initMenu(fonts)
    game.selectWin (m)
    game.start()
end
function runMod ()
    --game.logDebug("test mod: runMod()")
end
