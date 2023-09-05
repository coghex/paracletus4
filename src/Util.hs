module Util where
import Prelude()
import UPrelude
import Data.Time.Clock
import Data ( ID(..), Color(..) )
import Text.Printf ( printf )
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as BS64

-- | a simple head that returns maybe instead of crashing
head' ∷ [α] → Maybe α
head' list
  | not (null list) = Just $ head list
  | otherwise       = Nothing

-- | creates a unique id by hashing the system time, then base64 encoding
newID ∷ IO ID
newID = do
  t ← getCurrentTime
  let digest = MD5.finalize ctx
      ctx    = MD5.update ctx0 (BS.pack (show t))
      ctx0   = MD5.init
  return $ ID $ BS.unpack $ BS.cons '0' $ BS.cons 'x' $ BS64.encodeBase64' digest

-- | formats the fps Double to a string of set length n
showFPS ∷ Double → Int → String
showFPS fps len
  | fps ≥ (10^len) = show $ (10^len) - 1
  | fps ≤ 0        = replicate len ' '
  | otherwise      = stripLeadingZeros $ printf ("%0" ⧺ show len ⧺ "d") (round fps ∷ Int)
-- | strips a string of any leading zeros, replacing them with spaces
stripLeadingZeros ∷ String → String
stripLeadingZeros []        = []
stripLeadingZeros ('0':str) = ' ' : stripLeadingZeros str
stripLeadingZeros (ch:str)  = ch : str

-- | returns different colors
blackColor ∷ Color
blackColor = Color 255 255 255 255
whiteColor ∷ Color
whiteColor = Color 0   0   0   255
clearColor ∷ Color
clearColor = Color 0   0   0   0
redColor   ∷ Color
redColor   = Color 255 0   0   255
greenColor ∷ Color
greenColor = Color 0   255 0   255
blueColor  ∷ Color
blueColor  = Color 0   0   255 255
