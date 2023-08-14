require "mod/base/game"
JSON = (loadfile "mod/base/json.lua")()
function initTextures ()
    file = io.open ("mod/game/default/texture.json", "r+")
    ftype = io.type(file)
    if ftype == nil then
        -- create texture information
        game.logDebug("no texture data on file, creating texture data")
        local defaulTextureMap = JSON:encode( { textureData = texm } )
        outputf = io.open ("mod/game/default/texture.json", "w+")
        io.output(outputf)
        io.write(defaulTextureMap)
        io.close(outputf)
    else
        io.close(file)
    end
    game.registerTextureMap("mod/game/default/texture.json")
    return 0
end

texm = {}
texm[1] = { fp = "dat/tex/alpha.png",w = 1,h = 1 }
texm[2] = { fp = "dat/tex/texture.jpg",w = 1,h = 1 }
