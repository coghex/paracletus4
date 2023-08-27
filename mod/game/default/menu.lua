require "mod/base/game"
require "mod/base/util"
function initMenu ()
    menu = newWindow("menuWindow")
    t1   = newTile(0,0,1,1,menu,"grecoTile")
    t2   = newTile(3,3,1,1,menu,"grecoTile")
    a1   = newAtlas(8,0,2,2,menu,"plainsTile",1,1)
    x1   = newText(0,0,1,1,menu,"blop blop")
    return menu
end
