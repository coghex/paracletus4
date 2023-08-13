-- | fonts are loaded into memory and the font data is read off.
--   TODO: the old code here needs to be deleted
module Vulk.Font where
-- we can load unicode because this file does some silly stuff
--import Prelude()
--import UPrelude
import FreeType
import Control.Monad ( when )
import Data.Char ( ord )
import Data.Word ( Word8 )
import Foreign.Marshal.Array ( peekArray )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable ( Storable(peek) )

-- | a bitmap list of a font texture, giving width and height
data FontTex = FontTex Int Int [Word8]

-- | glyph metrics along with the index of
--   the texture, and the char value
data TTFData = TTFData
  { glVal ∷ Char
  , glInd ∷ Int
  , glMet ∷ GlyphMetrics
  } deriving (Show, Eq)

-- | glyph metrics are the ttf data associated with each char, google it
data GlyphMetrics = GlyphMetrics
  { gmW     ∷ Double
  , gmH     ∷ Double
  , gmX     ∷ Double
  , gmY     ∷ Double
  , gmA     ∷ Double
  } deriving (Show, Eq)

-- | finds character metrics for given char
indexTTFData ∷ [TTFData] → Char → Maybe TTFData
indexTTFData []     _  = Nothing
indexTTFData (d:ds) ch
  | glVal d == ch = Just d
  | otherwise     = indexTTFData ds ch

-- | puts a char of size px from ttf fp file pointer into a bitmap font
loadFTChar ∷ FilePath → Char → Int → IO (FontTex,TTFData)
loadFTChar fp char px = do
  ft_With_FreeType $ \lib →
    ft_With_Face lib fp 0 $ \face → do
      isScalable ← FT_IS_SCALABLE face
      when isScalable
        $ ft_Set_Char_Size face 0 (fromIntegral px * 64) 0 0
      ft_Load_Char face (fromIntegral $ ord char) FT_LOAD_RENDER
      slot ← peek . frGlyph =<< peek face
      withBitmap lib (gsrBitmap slot) $ \bmap → do
        let bufferSize
              = fromIntegral $ bRows bmap * fromIntegral (bPitch bmap)
        buffr ← peekArray bufferSize $ bBuffer bmap
        --drawBitmap (fromIntegral $ bPitch bmap) buffr
        let metrics = GlyphMetrics (w/dv) (h/dv) (x/dv) (y/dv) (a/dv)
            w       = fromIntegral $ gmWidth        $ gsrMetrics slot
            h       = fromIntegral $ gmHeight       $ gsrMetrics slot
            x       = fromIntegral $ gmHoriBearingX $ gsrMetrics slot
            y       = fromIntegral $ gmHoriBearingY $ gsrMetrics slot
            a       = fromIntegral $ gmHoriAdvance  $ gsrMetrics slot
            dv      = 4096.0
        return (FontTex (fromIntegral (bPitch bmap))
                        (fromIntegral (bRows bmap)) buffr
                        , TTFData char (indexTTF char) metrics)

-- | puts a bitmap font into memory
withBitmap ∷ FT_Library → FT_Bitmap → (FT_Bitmap → IO a) → IO a
withBitmap lib source f =
--  if any (== bPixel_mode source) -- below is hlint alternative
  if bPixel_mode source `elem`
       [ FT_PIXEL_MODE_MONO, FT_PIXEL_MODE_GRAY2
       , FT_PIXEL_MODE_GRAY4, FT_PIXEL_MODE_BGRA ]
    then ft_Bitmap_With lib $ \targetPtr → do
           with source $ \sourcePtr → do
             ft_Bitmap_Convert lib sourcePtr targetPtr
               . fromIntegral $ bPixel_mode source
             f =<< peek targetPtr
    else f source

-- | this silly function prints in unicode consoles only a bitmap
--   representation of a hinted character, left here as a curiosity
drawBitmap ∷ Int → [Word8] → IO ()
drawBitmap _ [] = return ()
drawBitmap n list = do
  putStrLn $ color <$> take n list
  drawBitmap n $ drop n list
  where
    color :: Word8 -> Char
    color a =
      case () of
        () | a == 0    -> ' '
           | a < 85    -> '░'
           | a < 170   -> '▒'
           | a < 255   -> '▓'
           | otherwise -> '█'

-- | specific font data encoded here, TODO: delete the junk here
chXUnit ∷ Double
chXUnit = 1.0 / 9.0
chYUnit ∷ Double
chYUnit = 1.0 / 7.0
indexTTF ∷ Char → Int -- TTFData
indexTTF '!' = 2
indexTTF '"' = 3
indexTTF '#' = 4
indexTTF '$' = 5
indexTTF '%' = 6
indexTTF '&' = 7
indexTTF '\'' = 8
indexTTF '(' = 9
indexTTF ')' = 10
indexTTF '*' = 11
indexTTF '+' = 12
indexTTF ',' = 13
indexTTF '-' = 14
indexTTF '.' = 15
indexTTF '/' = 16
indexTTF '0' = 17
indexTTF '1' = 18
indexTTF '2' = 19
indexTTF '3' = 20
indexTTF '4' = 21
indexTTF '5' = 22
indexTTF '6' = 23
indexTTF '7' = 24
indexTTF '8' = 25
indexTTF '9' = 26
indexTTF ':' = 27
indexTTF ';' = 28
indexTTF '<' = 29
indexTTF '=' = 30
indexTTF '>' = 31
indexTTF '?' = 32
indexTTF '@' = 33
indexTTF 'A' = 34
indexTTF 'B' = 35
indexTTF 'C' = 36
indexTTF 'D' = 37
indexTTF 'E' = 38
indexTTF 'F' = 39
indexTTF 'G' = 40
indexTTF 'H' = 41
indexTTF 'I' = 42
indexTTF 'J' = 43
indexTTF 'K' = 44
indexTTF 'L' = 45
indexTTF 'M' = 46
indexTTF 'N' = 47
indexTTF 'O' = 48
indexTTF 'P' = 49
indexTTF 'Q' = 50
indexTTF 'R' = 51
indexTTF 'S' = 52
indexTTF 'T' = 53
indexTTF 'U' = 54
indexTTF 'V' = 55
indexTTF 'W' = 56
indexTTF 'X' = 57
indexTTF 'Y' = 58
indexTTF 'Z' = 59
indexTTF '[' = 60
indexTTF '\\' = 61
indexTTF ']' = 62
indexTTF '^' = 63
indexTTF '_' = 64
indexTTF '`' = 65
indexTTF 'a' = 66
indexTTF 'b' = 67
indexTTF 'c' = 68
indexTTF 'd' = 69
indexTTF 'e' = 70
indexTTF 'f' = 71
indexTTF 'g' = 72
indexTTF 'h' = 73
indexTTF 'i' = 74
indexTTF 'j' = 75
indexTTF 'k' = 76
indexTTF 'l' = 77
indexTTF 'm' = 78
indexTTF 'n' = 79
indexTTF 'o' = 80
indexTTF 'p' = 81
indexTTF 'q' = 82
indexTTF 'r' = 83
indexTTF 's' = 84
indexTTF 't' = 85
indexTTF 'u' = 86
indexTTF 'v' = 87
indexTTF 'w' = 88
indexTTF 'x' = 89
indexTTF 'y' = 90
indexTTF 'z' = 91
indexTTF '{' = 92
indexTTF '|' = 93
indexTTF '}' = 94
indexTTF '~' = 95
indexTTF _   = 0
