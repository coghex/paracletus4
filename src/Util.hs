module Util where
import Prelude()
import UPrelude
import Data.Time.Clock
import Data ( ID(..) )
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64 as BS64
newID ∷ IO ID
newID = do
  t ← getCurrentTime
  let digest = MD5.finalize ctx
      ctx    = MD5.update ctx0 (BS.pack (show t))
      ctx0   = MD5.init
  return $ ID $ BS.unpack $ BS.cons '0' $ BS.cons 'x' $ BS64.encodeBase64' digest
