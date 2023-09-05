module Load.Util where

import Prelude ()
import Data ( ID(..), Color(..) )
import qualified Data.Map as Map
import Util ( blackColor )
import Vulk.Font ( indexTTFData, TTFData(..), GlyphMetrics(..) )
import UPrelude
import Load.Data
import qualified Vulk.GLFW as GLFW

-- | creates a number of empty tiles
emptyTiles ∷ Int → Int → [Tile]
emptyTiles n offset = take n $ repeat $ Tile IDNULL
                                             (TilePos (0,0) (4,4))
                                             (TileTex (0,0) (1,1) offset
                                             blackColor)
                                             (TileBhv False)

-- | pads a list of tiles with empty tiles
padTiles ∷ Int → Int → [Tile] → [Tile]
padTiles offset n tiles
  | length tiles < n = padding ⧺ tiles
  | otherwise        = tiles
      where padding = emptyTiles (n - length tiles) offset

-- | creates a box
boxTiles ∷ Int → (Double,Double) → (Double,Double)
  → (Int,Int) → Color → Bool → [Tile]
boxTiles fontsize pos scale (width, height) color moves = tiles'
  where width'         = 2.0 * fst scale * fromIntegral (width+1)
        height'        = 2.0 * snd scale * fromIntegral (height+1)
        postl          = pos
        postr          = ((fst pos) + width', snd pos)
        posbl          = (fst pos, (snd pos) - height')
        posbr          = ((fst pos) + width',(snd pos) - height')
        filltile       = Tile IDNULL (TilePos pos scale)
                                     (TileTex (0,0) (1,1) (fontsize-9) color)
                                     (TileBhv moves)
        righttile      = Tile IDNULL (TilePos postr scale)
                                     (TileTex (0,0) (1,1) (fontsize-8) color)
                                     (TileBhv moves)
        toptile        = Tile IDNULL (TilePos postl scale)
                                     (TileTex (0,0) (1,1) (fontsize-7) color)
                                     (TileBhv moves)
        toprtile       = Tile IDNULL (TilePos postr scale)
                                     (TileTex (0,0) (1,1) (fontsize-6) color)
                                     (TileBhv moves)
        topltile       = Tile IDNULL (TilePos postl scale)
                                     (TileTex (0,0) (1,1) (fontsize-5) color)
                                     (TileBhv moves)
        bottile        = Tile IDNULL (TilePos posbl scale)
                                     (TileTex (0,0) (1,1) (fontsize-4) color)
                                     (TileBhv moves)
        botrtile       = Tile IDNULL (TilePos posbr scale)
                                     (TileTex (0,0) (1,1) (fontsize-3) color)
                                     (TileBhv moves)
        botltile       = Tile IDNULL (TilePos posbl scale)
                                     (TileTex (0,0) (1,1) (fontsize-2) color)
                                     (TileBhv moves)
        lefttile       = Tile IDNULL (TilePos postl scale)
                                     (TileTex (0,0) (1,1) (fontsize-1) color)
                                     (TileBhv moves)
        toptiles       = tileHor width (fst scale) toptile
        bottiles       = tileHor width (fst scale) bottile
        lefttiles      = tileVer height (snd scale) lefttile
        righttiles     = tileVer height (snd scale) righttile
        fill           = tileFill width height scale filltile
        tiles'         = topltile : toprtile : botltile : botrtile
                       : (toptiles ⧺ bottiles ⧺ lefttiles ⧺ righttiles ⧺ fill)
tileHor ∷ Int → Double → Tile → [Tile]
tileHor 0 _     _    = []
tileHor n scale (Tile id0 (TilePos (x,y) (w,h)) tex mov) = tile' : tileHor (n-1) scale tile'
  where tile' = Tile id0 (TilePos (x+(2*scale),y) (w,h)) tex mov
tileVer ∷ Int → Double → Tile → [Tile]
tileVer 0 _     _    = []
tileVer n scale (Tile id0 (TilePos (x,y) (w,h)) tex mov) = tile' : tileVer (n-1) scale tile'
  where tile' = Tile id0 (TilePos (x,y-(2*scale)) (w,h)) tex mov
tileFill ∷ Int → Int → (Double,Double) → Tile → [Tile]
tileFill 0 _ _     _                                   = []
tileFill w h scale (Tile id0 (TilePos (x,y) sc) tex mov) = tiles ⧺ tileFill (w-1) h scale tile'
  where tile' = Tile id0 (TilePos (x+(2*fst scale),y) sc) tex mov
        tiles = tileVer h (snd scale) tile'

-- | generates the tiles for a single string
genStringTiles ∷ Int → Bool → [TTFData] → Double → (Double,Double)
  → (Double,Double) → String → [Tile]
genStringTiles _        _     _       _  _     _     []         = []
genStringTiles fontsize moves ttfdata x0 (x,y) (w,h) (' ':str)
  = genStringTiles fontsize moves ttfdata x0 (x+0.1,y) (w,h) str
genStringTiles fontsize moves ttfdata x0 (_,y) (w,h) ('\n':str)
  = genStringTiles fontsize moves ttfdata x0 (x0,y-1) (w,h) str
genStringTiles fontsize moves ttfdata x0 (x,y) (w,h) (ch:str)   = case indexTTFData ttfdata ch of
  Nothing → genStringTiles fontsize moves ttfdata x0 (x,y) (w,h) str
  Just (TTFData _ chInd (GlyphMetrics chW chH chX chY chA))
    → tile : genStringTiles fontsize moves ttfdata x0 (x+(w*2*chA),y) (w,h) str
      where tile = Tile IDNULL (TilePos (x',y') (w',h'))
                               (TileTex (0,0) (1,1) chInd' blackColor)
                               (TileBhv moves)
            (x',y') = (realToFrac(x+(2*w*chX)+w*chW)
                      ,realToFrac(y+(2*h*chY)-h*chH-0.1))
            (w',h') = (w * realToFrac chW
                      ,h * realToFrac chH)
            chInd'  = chInd + fontsize

-- | adds a winelem to the wins
addElemToWin ∷ Map.Map ID Window → ID → WinElem → Map.Map ID Window
addElemToWin wins win elem = case Map.lookup win wins of
  Nothing → wins
  Just w0 → Map.insert win win' wins
    where win' = w0 { winElems = elem : winElems w0 }

-- | converts tex to tiletex at input tex n
findTex ∷ String → [(String,Tex)] → TileTex
findTex _ [] = TileTex (0,0) (1,1) 0 blackColor
findTex n ((name,Tex _ ind siz):texs)
  | n ≡ name  = TileTex (0,0) siz ind blackColor
  | otherwise = findTex n texs
findAtlas ∷ Int → (Int,Int) → String → [(String,Tex)] → TileTex
findAtlas offset _    _ [] = TileTex (0,0) (1,1) offset blackColor
findAtlas offset tind n ((name,Tex _ ind siz):texs)
  | n ≡ name  = TileTex tind siz (ind+offset) blackColor
  | otherwise = findAtlas offset tind n texs

-- | gets the window size from the draw state's window variable
getWinSize ∷ Maybe GLFW.Window → IO (Int,Int)
getWinSize Nothing   = return (800,600)
getWinSize (Just w0) = GLFW.getWindowSize w0
