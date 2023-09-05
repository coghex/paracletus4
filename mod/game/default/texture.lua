require "mod/base/game"
JSON = (loadfile "mod/base/json.lua")()
function initTextures ()
    -- loads all textures in texture.json, loads the font in /dat/font
    font1 = game.loadFont("dat/font/asdf.ttf")
    font2 = game.loadFont("dat/font/oswald.ttf")
    file = io.open ("mod/game/default/texture.json", "r+")
    ftype = io.type(file)
    if ftype == nil then
        -- create texture information
        game.logDebug("no texture data on file, creating texture data")
        local defaulTextureMap = JSON:encode( { textureData = texm, atlasData = atlasm } )
        outputf = io.open ("mod/game/default/texture.json", "w+")
        io.output(outputf)
        io.write(defaulTextureMap)
        io.close(outputf)
    else
        io.close(file)
    end
    game.registerTextureMap("mod/game/default/texture.json")
    return {font1,font2}
end

texm = {}
texm[1] = { name = "alphaTile",fp = "dat/tex/alpha.png" }
texm[2] = { name = "grayscaleTile",fp = "dat/tex/grayscale.png" }
texm[3] = { name = "grecoTile",fp = "dat/tex/texture.jpg" }
texm[4] = { name = "blankPage",fp = "dat/tex/blankpage.jpg" }
texm[5] = { name = "nullTile",fp = "dat/tex/world/null.png" }

atlasm = {}
atlasm[1] = { name = "plainsTile",fp = "dat/tex/world/plains.png",w = 3, h = 4 }
