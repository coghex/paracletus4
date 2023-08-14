require "mod/base/game"
JSON = (loadfile "mod/base/json.lua")()
function initTiles ()
    file = io.open ("mod/game/default/tile.json", "r+")
    ftype = io.type(file)
    if ftype == nil then
        -- create tile mapping
        game.logDebug("no tilemap on file, creating tilemap")
        local defaulTileMap = JSON:encode( { tileMap=tm } )
        outputf = io.open ("mod/game/default/tile.json", "w+")
        io.output(outputf)
        io.write(defaulTileMap)
        io.close(outputf)
    else
        io.close(file)
    end
    game.registerTileMap("mod/game/default/tile.json")
    return 0
end

tm = { tiles = { tile01 = { t = 0 }
               , tile02 = { t = 1 } } }
