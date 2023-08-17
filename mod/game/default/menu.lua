require "mod/base/game"
require "mod/base/util"
function initMenu ()
    newTile(0,0,1,1,"grecoTile")
    newTile(3,3,1,1,"grecoTile")
    newAtlas(8,0,2,2,"plainsTile",1,1)
    return 0
end
