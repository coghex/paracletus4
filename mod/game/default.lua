-- defines the basic game
require "mod/base/game"
require "mod/base/json"
require "mod/game/default/menu"
require "mod/game/default/input"
require "mod/game/default/texture"
require "mod/game/default/tile"
-- this runs once at the beginning
function initMod ()
    -- basic UI elements
    game.logDebug("default mod: initMod()")
    initInput ()
    initTextures ()
    initTiles()
    initMenu ()
end
function runMod ()
    game.logDebug("default mod: runMod()")
end
