{-# LANGUAGE Strict #-}
-- | loads textures from listed files
--   TODO: this whole file is temporary, until
--         the lua interface is complete
module Vulk.VulkLoad where
-- loading textures from the lua files
-- is spun off as a child thread
import Prelude()
import UPrelude
import Control.Monad.State.Class (modify, gets)
import Prog ( MonadIO(liftIO), Prog, MonadReader(ask) )
import Sign.Var ( atomically )
import Sign.Data ( LoadCmd(..) )
import Sign.Util ( writeTVar', writeQueue', modifyTVar )
import Prog.Data ( State(..), TVarName(..), TVarValue(..)
                 , QueueName(..), QueueCmd(..) )
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
  -- mod textures get added in by the lua files
  modTexViews ← createTextureImageViews pdev dev cmdPool cmdQueue fps
  texSamplersMod ← createTextureSamplers dev $ snd . unzip $ modTexViews
  fontPath ← gets stFont
  (texViews,texSamps) ← case fontPath of
    Nothing → return (fst (unzip modTexViews), texSamplersMod)
    Just fp → do
      logDebug $ "[Vulk] loading font " ⧺ fp
      -- font texs are generated from ttf
      fontData ← createFontImageViews pdev dev cmdPool cmdQueue fp 16
      let (fontTexs, fontMetrics) = unzip fontData
          (ftexs, fmipLvls) = unzip fontTexs
      env ← ask
      modifyTVar env FontMapTVar $ TVFontMap fontMetrics
      fontSamplers ← createTextureSamplers dev fmipLvls
      -- box texs are for the shell
      let texBoxPath = "dat/tex/box"
      boxTexs ← loadNTexs pdev dev cmdPool cmdQueue texBoxPath
      let (btexs, bsamps) = unzip boxTexs
          len = (length ftexs) + (length btexs)
      modifyTVar env FontSizeTVar $ TVInt len
      writeQueue' LoadQueue $ QCLoadCmd $ LoadReload
      return (ftexs ⧺ btexs ⧺ (fst (unzip modTexViews))
             ,fontSamplers ⧺ bsamps ⧺ texSamplersMod)
  descriptorTextureInfo ← textureImageInfos texViews texSamps
  depthFormat ← findDepthFormat pdev
  let nimages = length texViews
  descriptorSetLayout ← createDescriptorSetLayout dev nimages
  pipelineLayout ← createPipelineLayout dev descriptorSetLayout
  let texdata = TextureData descriptorSetLayout pipelineLayout
                nimages descriptorTextureInfo depthFormat
  return texdata
