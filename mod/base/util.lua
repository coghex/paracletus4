game = game or {}

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
function game.start()
    rawStart()
end
function game.logDebug(str)
    logDebug(1,str)
end
function game.logInfo(str)
    logInfo(str)
end
function game.logError(str)
    logError(str)
end
function game.registerInputKeys(str)
    rawRegisterInputKeys(str)
end
function game.registerTileMap(str)
    rawRegisterTileMap(str)
end
function game.registerTextureMap(str)
    rawRegisterTextureMap(str)
end
function game.reload()
    rawReload()
end
function game.recreate()
    rawRecreate()
end
function game.selectWin(str)
    rawSelectWin(str)
end
function game.loadFont(str)
    rawLoadFont(str)
end
function game.initShell()
    rawInitShell()
end
function game.sleep(n)
    os.execute("sleep " .. tonumber(n))
end
function game.getWindowSize()
    size = rawGetWindowSize()
    return size
end
