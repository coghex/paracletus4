module Load.Util where

import Prelude ()
import Data ( ID(..), Color(..) )
import Util ( blackColor )
import Vulk.Font ( indexTTFData, TTFData(..), GlyphMetrics(..) )
import UPrelude
import Load.Data ( Tile(..), TilePos(..), TileTex(..) )

-- creates a number of empty tiles
emptyTiles ∷ Int → Int → [Tile]
emptyTiles n offset = take n $ repeat $ Tile IDNULL
                                             (TilePos (0,0) (4,4))
                                             (TileTex (0,0) (1,1) offset
                                             blackColor)

-- | creates a box
boxTiles ∷ Int → (Double,Double) → (Double,Double) → (Int,Int) → Color → [Tile]
boxTiles fontsize pos scale (width, height) color = tiles'
  where width'         = 2.0 * fst scale * fromIntegral (width+1)
        height'        = 2.0 * snd scale * fromIntegral (height+1)
        postl          = pos
        postr          = ((fst pos) + width', snd pos)
        posbl          = (fst pos, (snd pos) - height')
        posbr          = ((fst pos) + width',(snd pos) - height')
        filltile       = Tile IDNULL (TilePos pos scale)
                                     (TileTex (0,0) (1,1) (fontsize-9) color)
        righttile      = Tile IDNULL (TilePos postr scale)
                                     (TileTex (0,0) (1,1) (fontsize-8) color)
        toptile        = Tile IDNULL (TilePos postl scale)
                                     (TileTex (0,0) (1,1) (fontsize-7) color)
        toprtile       = Tile IDNULL (TilePos postr scale)
                                     (TileTex (0,0) (1,1) (fontsize-6) color)
        topltile       = Tile IDNULL (TilePos postl scale)
                                     (TileTex (0,0) (1,1) (fontsize-5) color)
        bottile        = Tile IDNULL (TilePos posbl scale)
                                     (TileTex (0,0) (1,1) (fontsize-4) color)
        botrtile       = Tile IDNULL (TilePos posbr scale)
                                     (TileTex (0,0) (1,1) (fontsize-3) color)
        botltile       = Tile IDNULL (TilePos posbl scale)
                                     (TileTex (0,0) (1,1) (fontsize-2) color)
        lefttile       = Tile IDNULL (TilePos postl scale)
                                     (TileTex (0,0) (1,1) (fontsize-1) color)
        toptiles       = tileHor width (fst scale) toptile
        bottiles       = tileHor width (fst scale) bottile
        lefttiles      = tileVer height (snd scale) lefttile
        righttiles     = tileVer height (snd scale) righttile
        fill           = tileFill width height scale filltile
        tiles'         = topltile : toprtile : botltile : botrtile
                       : (toptiles ⧺ bottiles ⧺ lefttiles ⧺ righttiles ⧺ fill)
tileHor ∷ Int → Double → Tile → [Tile]
tileHor 0 _     _    = []
tileHor n scale (Tile id0 (TilePos (x,y) (w,h)) tex) = tile' : tileHor (n-1) scale tile'
  where tile' = Tile id0 (TilePos (x+(2*scale),y) (w,h)) tex
tileVer ∷ Int → Double → Tile → [Tile]
tileVer 0 _     _    = []
tileVer n scale (Tile id0 (TilePos (x,y) (w,h)) tex) = tile' : tileVer (n-1) scale tile'
  where tile' = Tile id0 (TilePos (x,y-(2*scale)) (w,h)) tex
tileFill ∷ Int → Int → (Double,Double) → Tile → [Tile]
tileFill 0 _ _     _                                   = []
tileFill w h scale (Tile id0 (TilePos (x,y) sc) tex) = tiles ⧺ tileFill (w-1) h scale tile'
  where tile' = Tile id0 (TilePos (x+(2*fst scale),y) sc) tex
        tiles = tileVer h (snd scale) tile'

-- | generates the tiles for a single string
genStringTiles ∷ Int → [TTFData] → Double → (Double,Double)
  → (Double,Double) → String → [Tile]
genStringTiles _        _       _  _     _     []         = []
genStringTiles fontsize ttfdata x0 (x,y) (w,h) (' ':str)
  = genStringTiles fontsize ttfdata x0 (x+0.1,y) (w,h) str
genStringTiles fontsize ttfdata x0 (_,y) (w,h) ('\n':str)
  = genStringTiles fontsize ttfdata x0 (x0,y-1) (w,h) str
genStringTiles fontsize ttfdata x0 (x,y) (w,h) (ch:str)   = case indexTTFData ttfdata ch of
  Nothing → genStringTiles fontsize ttfdata x0 (x,y) (w,h) str
  Just (TTFData _ chInd (GlyphMetrics chW chH chX chY chA))
    → tile : genStringTiles fontsize ttfdata x0 (x+(w*2*chA),y) (w,h) str
      where tile = Tile IDNULL (TilePos (x',y') (w',h'))
                               (TileTex (0,0) (1,1) chInd' blackColor)
            (x',y') = (realToFrac(x+(2*w*chX)+w*chW)
                      ,realToFrac(y+(2*h*chY)-h*chH-0.1))
            (w',h') = (w * realToFrac chW
                      ,h * realToFrac chH)
            chInd'  = chInd + fontsize
