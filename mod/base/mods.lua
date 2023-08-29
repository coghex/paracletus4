require "mod/base/util"

JSON = (loadfile "mod/base/json.lua")()
function initModRankings (str)
    file = io.open ("mod/base/mods.json","r+")
    ftype = io.type (file)
    if ftype == nil then
        game.logDebug("no mod load order, calculating...")
        orderedfiles = orderMods(str)
        local defaultModOrder = JSON:encode (  orderedfiles, { pretty = true } )
        outputf = io.open ( "mod/base/mods.json", "w+")
        io.output(outputf)
        io.write(defaultModOrder)
        io.close(outputf)
    else
        io.close(file)
    end
    return 0
end
function orderMods(str)
    -- decode haskell string
    inp = {}
    for file,k in string.gmatch(str, "([^;]*)") do
        table.insert(inp,file)
    end
    -- create sortable list of indices
    keys = {}
    for key,_ in pairs(inp) do
        table.insert(keys,key)
    end
    -- sort the key list
    table.sort(keys,function(left,right)
        if inp[left] == "mod/game/default.lua" then
            return true
        else return false
        end
      end
    )
    -- insert into a new output table
    outp = {}
    for _,k0 in ipairs(keys) do
        table.insert(outp,inp[k0])
    end
    return outp
end
