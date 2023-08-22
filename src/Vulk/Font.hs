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
indexTTF '!' = 0
indexTTF '"' = 1
indexTTF '#' = 2
indexTTF '$' = 3
indexTTF '%' = 4
indexTTF '&' = 5
indexTTF '\'' = 6
indexTTF '(' = 7
indexTTF ')' = 8
indexTTF '*' = 9
indexTTF '+' = 10
indexTTF ',' = 11
indexTTF '-' = 12
indexTTF '.' = 13
indexTTF '/' = 14
indexTTF '0' = 15
indexTTF '1' = 16
indexTTF '2' = 17
indexTTF '3' = 18
indexTTF '4' = 19
indexTTF '5' = 20
indexTTF '6' = 21
indexTTF '7' = 22
indexTTF '8' = 23
indexTTF '9' = 24
indexTTF ':' = 25
indexTTF ';' = 26
indexTTF '<' = 27
indexTTF '=' = 28
indexTTF '>' = 29
indexTTF '?' = 30
indexTTF '@' = 31
indexTTF 'A' = 32
indexTTF 'B' = 33
indexTTF 'C' = 34
indexTTF 'D' = 35
indexTTF 'E' = 36
indexTTF 'F' = 37
indexTTF 'G' = 38
indexTTF 'H' = 39
indexTTF 'I' = 40
indexTTF 'J' = 41
indexTTF 'K' = 42
indexTTF 'L' = 43
indexTTF 'M' = 44
indexTTF 'N' = 45
indexTTF 'O' = 46
indexTTF 'P' = 47
indexTTF 'Q' = 48
indexTTF 'R' = 49
indexTTF 'S' = 50
indexTTF 'T' = 51
indexTTF 'U' = 52
indexTTF 'V' = 53
indexTTF 'W' = 54
indexTTF 'X' = 55
indexTTF 'Y' = 56
indexTTF 'Z' = 57
indexTTF '[' = 68
indexTTF '\\' = 59
indexTTF ']' = 60
indexTTF '^' = 61
indexTTF '_' = 62
indexTTF '`' = 63
indexTTF 'a' = 64
indexTTF 'b' = 65
indexTTF 'c' = 66
indexTTF 'd' = 67
indexTTF 'e' = 68
indexTTF 'f' = 69
indexTTF 'g' = 70
indexTTF 'h' = 71
indexTTF 'i' = 72
indexTTF 'j' = 73
indexTTF 'k' = 74
indexTTF 'l' = 75
indexTTF 'm' = 76
indexTTF 'n' = 77
indexTTF 'o' = 78
indexTTF 'p' = 79
indexTTF 'q' = 80
indexTTF 'r' = 81
indexTTF 's' = 82
indexTTF 't' = 83
indexTTF 'u' = 84
indexTTF 'v' = 85
indexTTF 'w' = 86
indexTTF 'x' = 87
indexTTF 'y' = 88
indexTTF 'z' = 89
indexTTF '{' = 90
indexTTF '|' = 91
indexTTF '}' = 92
indexTTF '~' = 93
indexTTF _   = 94
