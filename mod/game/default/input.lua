require "mod/base/game"
JSON = (loadfile "mod/base/json.lua")()
function initInput ()
    file = io.open ("mod/game/default/input.json", "r+")
    ftype = io.type(file)
    if ftype == nil then
        -- if there are no input settings then we will
        -- need to create them
        game.logDebug("no settings on file, creating settings")
        local defaulInputSettings = JSON:encode( { keySettings=keySetts }, { pretty = true } )
        outputf = io.open ("mod/game/default/input.json", "w+")
        io.output(outputf)
        io.write(defaulInputSettings)
        io.close(outputf)
    else
        io.close(file)
    end
    game.registerInputKeys("mod/game/default/input.json")
    return 0
end

keySetts = { keyEscape = "ESC,CAP"
           , keyTest   = "T"
           , keyShell  = "`" }
