function newTile(x,y,w,h,win,t)
    id = rawNewTile(x,y,w,h,win,t)
    return id
end

function newAtlas(x,y,w,h,win,t,tx,ty)
    id = rawNewAtlas(x,y,w,h,win,t,tx,ty)
    return id
end

function newWindow(str)
    id = rawNewWindow(str)
    return id
end
