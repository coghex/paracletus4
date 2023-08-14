require "mod/base/game"
JSON = (loadfile "mod/base/json.lua")()
function initInput ()
    file = io.open ("mod/game/default/input.json", "r+")
    ftype = io.type(file)
    if ftype == nil then
        -- if there are no input settings then we will
        -- need to create them
        game.logDebug("no settings on file, creating settings")
        local defaulInputSettings = JSON:encode( { KeySettings=keySettings } )
        outputf = io.open ("mod/game/default/input.json", "w+")
        io.output(outputf)
        io.write(defaulInputSettings)
        io.close(outputf)
    end
    io.close(file)
    game.registerInputKeys()
    return 0
end

keySettings = { KeyEscape = "ESC"
              , KeyTest   = "T" }
