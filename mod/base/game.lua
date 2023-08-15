-- this runs once at the beginning
function initLuau (files)
    for file,k in string.gmatch(files, "([^;]*)") do
      local f = assert(loadfile(file))
      f ()
      initMod ()
    end
    return 0
end
-- this runs every tick
function runLuau (files)
    for file,k in string.gmatch(files, "([^;]*)") do
      local f = assert(loadfile(file))
      f ()
      runMod ()
    end
    return 0
end

game = game or {}

function game.logDebug(str)
    logDebug(1,str)
end
function game.logInfo(str)
    logInfo(str)
end
function game.logError(str)
    logError(str)
end
function game.registerInputKeys(str)
    rawRegisterInputKeys(str)
end
function game.registerTileMap(str)
    rawRegisterTileMap(str)
end
function game.registerTextureMap(str)
    rawRegisterTextureMap(str)
end
function game.reload()
    rawReload()
end
