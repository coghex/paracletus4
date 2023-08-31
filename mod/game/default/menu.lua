require "mod/base/game"
require "mod/base/util"
function initMenu (fonts)
    menu   = newWindow("menuWindow")
    s      = game.getWindowSize()
    w      = s[1] / 64
    h      = s[2] / 64
    bkgrnd = newTile(0,0,w,h,loadingscreen,"blankPage")
    a1     = newAtlas(8,0,2,2,menu,"plainsTile",1,1)
    x1     = newText(w-2,h-2,1,1,menu,fonts[2],"blop blop")
    return menu
end
