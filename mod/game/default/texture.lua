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
texm[1] = { name = "alphaTile",fp = "dat/tex/alpha.png" }
texm[2] = { name = "grecoTile",fp = "dat/tex/texture.jpg" }
texm[3] = { name = "grayscaleTile",fp = "dat/tex/grayscale.png" }
