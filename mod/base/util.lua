require "mod/base/game"

function newTile(x,y,w,h,win,t)
    id = rawNewTile(x,y,w,h,win,t)
    game.recreate()
    return id
end

function newAtlas(x,y,w,h,win,t,tx,ty)
    id = rawNewAtlas(x,y,w,h,win,t,tx,ty)
    game.recreate()
    return id
end

function newWindow(str)
    id = rawNewWindow(str)
    game.recreate()
    return id
end

function newText (x,y,w,h,win,text)
    id = rawNewText (x,y,w,h,win,text)
    game.recreate()
    return id
end

-- turns a table into a string
function dump(o)
   if type(o) == 'table' then
      local s = '{ '
      for k,v in pairs(o) do
         if type(k) ~= 'number' then k = '"'..k..'"' end
         s = s .. '['..k..'] = ' .. dump(v) .. ','
      end
      return s .. '} '
   else
      return tostring(o)
   end
end
