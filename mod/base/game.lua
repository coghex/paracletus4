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

loaded = 0
