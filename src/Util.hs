module Util where
import Prelude()
import UPrelude
import Data.Time.Clock
import Data ( ID(..), Color(..) )
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as BS64

-- | creates a unique id by hashing the system time, then base64 encoding
newID ∷ IO ID
newID = do
  t ← getCurrentTime
  let digest = MD5.finalize ctx
      ctx    = MD5.update ctx0 (BS.pack (show t))
      ctx0   = MD5.init
  return $ ID $ BS.unpack $ BS.cons '0' $ BS.cons 'x' $ BS64.encodeBase64' digest

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
