-- | indexes height and width into a texture atlas, this is where
--   a huge portion of the slowdown from swapchain recreation comes from
module Vulk.Atlas where
-- we have some indexing functions
import Prelude()
import UPrelude
import Graphics.Vulkan.Core_1_0
    ( VkPhysicalDevice,
      VkDevice,
      VkCommandPool,
      VkQueue,
      VkImageView,
      VkSampler )
import Numeric.DataFrame ( Vec3f, DataFrame(Vec3), Vector3(vec3) )
import Prog ( Prog )
import Vulk.Texture
    ( createTextureImageView, createTextureSampler )

-- | an atlas contains a textures size, view, and sampler, along
--   with a layout of the size of the thing
data Atlas = Atlas { twidth  ∷ Int -- ^ texture width in segments
                   , theight ∷ Int -- ^ texture height in segments
                   , swidth  ∷ Int -- ^ segment width in pixels
                   , sheight ∷ Int -- ^ segment height in pixels
                   , imgview ∷ VkImageView -- ^ vulkan texture
                   , sampler ∷ VkSampler } -- ^ vulkan sampler

-- | a wrapper function to create an atlas as
--   a texture and sampler with data
createAtlas ∷ VkPhysicalDevice → VkDevice → VkCommandPool → VkQueue
  → (Int, Int) → (Int, Int) → String → Prog ε σ Atlas
createAtlas pdev dev commandPool queue (tw, th) (sw, sh) texPath = do
    (textureView, mipLevels)
      ← createTextureImageView pdev dev commandPool queue texPath
    textureSampler ← createTextureSampler dev mipLevels
    return Atlas { twidth  = tw
                 , theight = th
                 , swidth  = sw
                 , sheight = sh
                 , imgview = textureView
                 , sampler = textureSampler }

-- | this is where the computational stutter occurs
--   during swapchain recreation
-- TODO: speed this up, possible with inlines and strictness
indexAtlas ∷ Int → Int → Int → Int → Vec3f → Vec3f
indexAtlas i j w h (Vec3 x y n) = vec3 x' y' n
  where x' = (i' + x) / w'
        y' = (j' + y) / h'
        i' = fromIntegral i
        j' = fromIntegral j
        w' = fromIntegral w
        h' = fromIntegral h
