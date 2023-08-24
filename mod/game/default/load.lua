require "mod/base/game"
require "mod/base/util"
function initLoadScreen ()
    loadingscreen = newWindow("loadWindow")
    loadtile1 = newTile(0,0,1,1,loadingscreen,"grecoTile")
    return loadingscreen
end
