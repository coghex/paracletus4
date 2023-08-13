{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
-- function to transform data from one frame to the next
module Vulk.Trans where
-- dynamic data is applied
import Prelude()
import UPrelude
import Control.Monad (replicateM)
import Data ( Color(..) )
import Foreign.Ptr (castPtr,plusPtr)
import GHC.Generics (Generic)
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create ( (&*), createVk, set )
import Numeric.DataFrame
    ( PrimBytes,
      DataFrame(DF4),
      (%*),
      scalar,
      bSizeOf,
      HomTransform4(orthogonal, translate3),
      Mat44f,
      Vector3(vec3) )
import Load.Data ( DynData(..) )
import Prog ( MonadIO(liftIO), Prog )
import Prog.Foreign ( allocaPeek, poke )
import Vulk.Foreign ( runVk )
import Vulk.Buff ( createBuffer )
import Vulk.Data ()

-- | a global transformation object, i cant really
--   think of a use for it but its kept because vulkan
--   is smart enough to make it read only, and all vert
--   threads can read it from one place at the same time.
data TransformationObject = TransformationObject
  { model ∷ Mat44f
  , view  ∷ Mat44f
  , proj  ∷ Mat44f
  } deriving (Show, Generic)
instance PrimBytes TransformationObject
transObjSize = bSizeOf @TransformationObject undefined ∷ VkDeviceSize

-- | a matrix representing data sent over the descriptor
--   hlint says a newtype is ok here
newtype DynTexTransObject = DynTexTransObject
  { dtexi ∷ Mat44f
  } deriving (Show, Generic)
instance PrimBytes DynTexTransObject
dynTexTransObjSize = bSizeOf @DynTexTransObject undefined ∷ Int
dynTexTransObjSize' = bSizeOf @DynTexTransObject undefined :: VkDeviceSize

-- | a matrix representing physical translation of an object
--   hlint says a newtype is ok here
newtype DynTransObject = DynTransObject
  { move ∷ Mat44f
  } deriving (Show, Generic)
instance PrimBytes DynTransObject
dynTransObjSize = bSizeOf @DynTransObject undefined ∷ Int
dynTransObjSize' = bSizeOf @DynTransObject undefined ∷ VkDeviceSize


-- | the global transformation object is updated, at the moment it
--   never changes, and only defines the general camera position
updateTransObj ∷ (Double,Double,Double) → VkDevice → VkExtent2D
  → VkDeviceMemory → Prog ε σ ()
updateTransObj (cx,cy,cz) device extent uniBuf = do
  uboPtr ← allocaPeek $ runVk ∘ vkMapMemory device uniBuf 0
    transObjSize VK_ZERO_FLAGS
  let model = DF4
                (DF4 32 0 0 0)
                (DF4 0 32 0 0)
                (DF4 0 0 32 0)
                (DF4 0 0 0  1)
  poke (castPtr uboPtr) (scalar $ TransformationObject {..})
  liftIO $ vkUnmapMemory device uniBuf
  -- these commands are all backwards
  -- ortho near far w h
  where view = translate3 (vec3 x y z)
        (x,y,z) = (realToFrac cx, realToFrac cy, realToFrac cz)
        proj  = proj' %* clip
        proj' = orthogonal 0.1 500 (fromIntegral width)
                                  (fromIntegral height)
        --proj' = perspective 0.1 500 (45/360*2*pi) aspectRatio
        clip = DF4
          (DF4 1   0   0   0)
          (DF4 0 (-1)  0   0)
          (DF4 0   0  0.5  0)
          (DF4 0   0  0.5  1)
        width = getField @"width" extent
        height = getField @"height" extent
        --aspectRatio = fromIntegral width / fromIntegral height

-- | this is where we loop over all dynamic data and apply it to
--   the descriptor data locations
updateTransDyn ∷ Int → [DynData] → VkDevice → VkExtent2D
  → VkDeviceMemory → Prog ε σ ()
updateTransDyn _    []       _      _      _      = return ()
updateTransDyn nDyn dyns device _      uniBuf = do
  let nDyn'   = fromIntegral nDyn
  uboPtr ← allocaPeek $ runVk ∘ vkMapMemory device
    uniBuf 0 (nDyn'*dynTransObjSize') VK_ZERO_FLAGS
  let updateTransDynFunc ∷ Int → [DynData] → Ptr α → Prog ε σ ()
      updateTransDynFunc _    []       _        = return ()
      updateTransDynFunc nDyn0 (dd:dds) uboPtr0 = do
        let move = ddDataF dd
        -- let move = case ddDataF dd of
        --               Just dataf0 → dataf0
        --               Nothing     → dynDataFrame (ddPos dd) (ddScale dd)
--        let move = DF4
--              (DF4 w 0 0 0)
--              (DF4 0 h 0 0)
--              (DF4 0 0 1 0)
--              (DF4 x y 0 1)
--            (x ,y)  = (realToFrac x', realToFrac y')
--            (x',y') = ddPos dd
--            (w ,h)  = (realToFrac w', realToFrac h')
--            (w',h') = ddScale dd
            nDyn0'  = nDyn0 - 1
        poke (plusPtr (castPtr uboPtr0)
          (nDyn0'*dynTransObjSize))
          (scalar $ DynTransObject move)
        updateTransDynFunc nDyn0' dds uboPtr0
  updateTransDynFunc nDyn dyns uboPtr
  liftIO $ vkUnmapMemory device uniBuf

-- | construct datafram for dyn data
dynDataFrame ∷ (Float,Float) → (Float,Float) → Mat44f
dynDataFrame (x',y') (w',h') = DF4
    (DF4 w 0 0 0)
    (DF4 0 h 0 0)
    (DF4 0 0 1 0)
    (DF4 x y 0 1)
  where (x ,y)  = (realToFrac x', realToFrac y')
        (w ,h)  = (realToFrac w', realToFrac h')

-- | updates the texture data allowing us to cycle through textures
--   on the same vertex data, we use this matrix for other data too
updateTransTex ∷ Int → [DynData] → VkDevice → VkExtent2D
  → VkDeviceMemory → Prog ε σ ()
updateTransTex _    []   _      _      _      = return ()
updateTransTex nDyn dyns device _      uniBuf = do
  let nDyn'   = fromIntegral nDyn
  uboPtr ← allocaPeek $ runVk ∘ vkMapMemory device uniBuf 0
    (nDyn'*dynTexTransObjSize') VK_ZERO_FLAGS
  let updateTransTexFunc ∷ Int → [DynData] → Ptr α → Prog ε σ ()
      updateTransTexFunc _     []       _       = return ()
      updateTransTexFunc nDyn0 (dd:dds) uboPtr0 = do
        let dtexi = ddTexDF dd
        --let dtexi = case ddTexDF dd of
        --              Just dataf0 → dataf0
        --              Nothing → texDynDataFrame (ddColor dd) (ddTIndex dd) (ddTex dd)
 --                     Nothing → DF4
 --                       (DF4 1 0 0 r)
 --                       (DF4 0 1 0 g)
 --                       (DF4 0 0 1 b)
 --                       (DF4 x y n a)
 --           Color r' g' b' a' = ddColor dd
 --           (r,g,b,a) = (fromIntegral r', fromIntegral g'
 --                       ,fromIntegral b', fromIntegral a')
 --           (x ,y)    = (fromIntegral x', fromIntegral y')
 --           (x',y')   = ddTIndex dd
 --           n         = fromIntegral $ ddTex dd
            nDyn0'    = nDyn0 - 1
        poke (plusPtr (castPtr uboPtr0)
          (nDyn0'*dynTexTransObjSize))
          (scalar $ DynTexTransObject dtexi)
        updateTransTexFunc nDyn0' dds uboPtr0
  updateTransTexFunc nDyn dyns uboPtr
  liftIO $ vkUnmapMemory device uniBuf

-- | constructs dataframe for tex data
texDynDataFrame ∷ Color → (Int,Int) → Int → Mat44f
texDynDataFrame (Color r' g' b' a') (x',y') n' = DF4
    (DF4 1 0 0 r)
    (DF4 0 1 0 g)
    (DF4 0 0 1 b)
    (DF4 x y n a)
    where (x,y)     = (fromIntegral x', fromIntegral y')
          n         = fromIntegral n'
          (r,g,b,a) = (fromIntegral r', fromIntegral g'
                      ,fromIntegral b', fromIntegral a')

-- | initialization of the trans object buffers
createTransObjBuffers ∷ VkPhysicalDevice → VkDevice → Int
  → Prog ε σ [(VkDeviceMemory, VkBuffer)]
createTransObjBuffers pdev dev n = replicateM n $ createBuffer
  pdev dev transObjSize
  VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
  (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
  ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)

-- | transobjbuffer is a uniformbuffer
transObjBufferInfo ∷ VkBuffer → Prog ε σ VkDescriptorBufferInfo
transObjBufferInfo uniformBuffer
  = return $ createVk @VkDescriptorBufferInfo
    $  set @"buffer" uniformBuffer
    &* set @"offset" 0
    &* set @"range" transObjSize

-- | initialization of the trans dyn buffers
createTransDynBuffers ∷ VkPhysicalDevice → VkDevice → Int → Int
  → Prog ε σ [(VkDeviceMemory, VkBuffer)]
createTransDynBuffers pdev dev n nDyn = replicateM n $ createBuffer
  pdev dev (nDyn'*dynTransObjSize')
  VK_BUFFER_USAGE_STORAGE_BUFFER_BIT
  (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
  ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
  where nDyn' = max 1 $ fromIntegral nDyn

-- | transdynbuffer is a uniformbuffer
transDynBufferInfo ∷ Int → VkBuffer → Prog ε σ VkDescriptorBufferInfo
transDynBufferInfo nDyn uniformBuffer
  = return $ createVk @VkDescriptorBufferInfo
    $  set @"buffer" uniformBuffer
    &* set @"offset" 0
    &* set @"range" (nDyn'*dynTransObjSize')
    where nDyn' = max 1 $ fromIntegral nDyn

-- | initialization of the trans tex buffers
createTransTexBuffers ∷ VkPhysicalDevice → VkDevice → Int → Int
  → Prog ε σ [(VkDeviceMemory, VkBuffer)]
createTransTexBuffers pdev dev n nDyn = replicateM n $ createBuffer
  pdev dev (nDyn'*dynTexTransObjSize')
  VK_BUFFER_USAGE_STORAGE_BUFFER_BIT
  (VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
  ⌄ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
  where nDyn' = max 1 $ fromIntegral nDyn

-- | transtexbuffer is a uniformbuffer
transTexBufferInfo ∷ Int → VkBuffer → Prog ε σ VkDescriptorBufferInfo
transTexBufferInfo nDyn uniformBuffer
  = return $ createVk @VkDescriptorBufferInfo
    $  set @"buffer" uniformBuffer
    &* set @"offset" 0
    &* set @"range" (nDyn'*dynTexTransObjSize')
    where nDyn' = max 1 $ fromIntegral nDyn
