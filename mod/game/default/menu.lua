require "mod/base/game"
require "mod/base/util"
function initMenu (fonts)
    menu     = newWindow("menuWindow")
    loadwin  = newWindow("loadWindow")
    gamewin  = newWindow("gameWindow")
    s        = game.getWindowSize()
    w        = s[1] / 64
    h        = s[2] / 64
 --   bkgrnd   = newTile(0,0,w,h,menu,"blankPage")
    t1       = newText(2-w,h-2,3,3,menu,fonts[2],"a bridge far away...")
    x1       = newLink(2-w,h-4,1,1,menu,fonts[1],"new game",gamewin)
    x2       = newLink(2-w,h-5,1,1,menu,fonts[1],"load game",loadwin)
    w0       = game.newWorld(gamewin)
    return menu
end
