require "mod/base/game"
require "mod/base/util"
function initLoadScreen ()
    loadingscreen = newWindow("loadWindow")
    loadtile1 = newTile(0,0,10,10,loadingscreen,"blankPage")
    return loadingscreen
end
