{-# LANGUAGE Strict #-}
-- | loads textures from listed files
--   TODO: this whole file is temporary, until
--         the lua interface is complete
module Vulk.VulkLoad where
-- loading textures from the lua files
-- is spun off as a child thread
import Prelude()
import UPrelude
import Control.Monad.State.Class (modify)
import Prog ( MonadIO(liftIO), Prog, MonadReader(ask) )
import Sign.Var ( atomically, modifyTVar' )
import Prog.Util ( logDebug )
import Vulk.Data ( TextureData(TextureData) )
import Vulk.VulkData ( GQData(GQData) )
import Vulk.Desc ( createDescriptorSetLayout )
import Vulk.Texture
    ( createFontImageViews,
      createTextureImageView,
      createTextureImageViews,
      createTextureSampler,
      createTextureSamplers,
      findDepthFormat,
      loadNTexs,
      textureImageInfos )
import Vulk.Pipeline ( createPipelineLayout )

-- | loads all the textures into layouts of
--   the descriptor sets and pipeline. an
--   empty string will just load default textures
--   and filepaths added will be ammended to that
--   if you dont load more than 3 textures this will
--   throw a vulkan error, i have no idea why
loadVulkanTextures ∷ GQData → [FilePath] → Prog ε σ (TextureData)
loadVulkanTextures (GQData pdev dev cmdPool cmdQueue) fps = do
  -- the engine reserves the first few
  -- textures for default usage.
  let tex0Path     = "dat/tex/alpha.png"
      tex1Path     = "dat/tex/texture.jpg"
  -- tex zero is just 32x32 alpha
  (textureView0, mipLevels0)
    ← createTextureImageView pdev dev cmdPool cmdQueue tex0Path
  textureSampler0 ← createTextureSampler dev mipLevels0
  -- tex one is the vulkan tutorial image
  (textureView1, mipLevels1)
    ← createTextureImageView pdev dev cmdPool cmdQueue tex1Path
  textureSampler1 ← createTextureSampler dev mipLevels1
  -- mod textures get added in by the lua files
  modTexViews ← createTextureImageViews pdev dev cmdPool cmdQueue fps
  texSamplersMod ← createTextureSamplers dev $ snd . unzip $ modTexViews
  let texViews = fst (unzip modTexViews)
      texSamps = texSamplersMod
  descriptorTextureInfo ← textureImageInfos texViews texSamps
  depthFormat ← findDepthFormat pdev
  let nimages = length texViews
  descriptorSetLayout ← createDescriptorSetLayout dev nimages
  pipelineLayout ← createPipelineLayout dev descriptorSetLayout
  let texdata = TextureData descriptorSetLayout pipelineLayout
                nimages descriptorTextureInfo depthFormat
  return texdata
