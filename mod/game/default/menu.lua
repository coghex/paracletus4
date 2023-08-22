require "mod/base/game"
require "mod/base/util"
function initMenu ()
    win1 = newWindow("menuWindow")
    t1 = newTile(0,0,1,1,win1,"grecoTile")
    t2 = newTile(3,3,1,1,win1,"grecoTile")
    a1 = newAtlas(8,0,2,2,win1,"plainsTile",1,1)
    x1 = newText(0,0,1,1,win1,"blop blop")
    win2 = newWindow("new...")
    t3 = newTile(5,0,1,1,win2,"grecoTile")
    return win1
end
