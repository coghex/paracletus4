require "mod/base/game"
require "mod/base/util"
function initMenu (fonts)
    menu     = newWindow("menuWindow")
    gamewin  = newWindow("gameWindow")
    s        = game.getWindowSize()
    w        = s[1] / 64
    h        = s[2] / 64
    bkgrnd   = newTile(0,0,w,h,menu,"blankPage")
    x1       = newText(2-w,h-2,1,1,menu,fonts[2],"a bridge far away...")
    x2       = newLink(2-w,h-3,1,1,menu,fonts[1],"new game",gamewin)
    gametile = newTile(0,0,1,1,gamewin,"grecoTile")
    return menu
end
