require "mod/base/game"
require "mod/base/util"
function initLoadScreen ()
    loadingscreen = newWindow("loadWindow")
    s    = game.getWindowSize()
    w    = s[1] / 64
    h    = s[2] / 64
    loadtile1 = newTile(0,0,w,h,loadingscreen,"blankPage")
    loadtext  = newText(0,0,2,2,loadingscreen,"loading...")
    return loadingscreen
end
