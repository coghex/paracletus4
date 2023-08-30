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
import Control.Monad ( when )
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
import Vulk.Font ( Font(..), TTFData(..) )
import Graphics.Vulkan

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
--      modifyTVar env FontMapTVar $ TVFontMap fontMetrics
--      fontSamplers ← createTextureSamplers dev fmipLvls
--      -- box texs are for the shell
--      let texBoxPath = "dat/tex/box"
--      boxTexs ← loadNTexs pdev dev cmdPool cmdQueue texBoxPath
--      let (btexs, bsamps) = unzip boxTexs
--          len = (length ftexs) + (length btexs)
--      modifyTVar env FontSizeTVar $ TVInt len
--      writeQueue' LoadQueue $ QCLoadCmd $ LoadReload
--      return (ftexs ⧺ btexs ⧺ (fst (unzip modTexViews))
--             ,fontSamplers ⧺ bsamps ⧺ texSamplersMod)
  (ftexs,fsamps,fmets,fonts) ← loadFontTextures (GQData pdev dev cmdPool cmdQueue)
  (btexs,bsamps) ← loadUtilTextures (GQData pdev dev cmdPool cmdQueue)
  let len = (length (flatten ftexs)) + (length btexs)
      (ftexs',fsamps') = (flatten ftexs, flatten fsamps)
      (texViews,texSamps) = (ftexs' ⧺ btexs ⧺ (map fst modTexViews)
                            ,fsamps' ⧺ bsamps ⧺ texSamplersMod)
  env ← ask
  when (len > length btexs) $ do
    modifyTVar env FontSizeTVar $ TVInt len
    modifyTVar env FontMapTVar $ TVFontMap fmets
    modify $ \s → s { stFont = fonts }
    modifyTVar env FontsTVar $ TVFonts fonts
  writeQueue' LoadQueue $ QCLoadCmd LoadReload
  descriptorTextureInfo ← textureImageInfos texViews texSamps
  depthFormat ← findDepthFormat pdev
  let nimages = length texViews
  descriptorSetLayout ← createDescriptorSetLayout dev nimages
  pipelineLayout ← createPipelineLayout dev descriptorSetLayout
  let texdata = TextureData descriptorSetLayout pipelineLayout
                nimages descriptorTextureInfo depthFormat
  return texdata

loadFontTextures ∷ GQData → Prog ε σ ([[VkImageView]],[[VkSampler]],[[TTFData]],[Font])
loadFontTextures gqdata = do
  fonts ← gets stFont
  loadFontTexturesF 0 fonts gqdata
loadFontTexturesF ∷ Int → [Font] → GQData → Prog ε σ ([[VkImageView]],[[VkSampler]]
                                               ,[[TTFData]],[Font])
loadFontTexturesF _ []     _ = return ([],[],[],[])
loadFontTexturesF n ((Font fp id _ _):fs)
  (GQData pdev dev cmdPool cmdQueue) = do
    logDebug $ "[Vulk] loading font " ⧺ fp
    -- font texs are generated from ttf
    fontData     ← createFontImageViews pdev dev cmdPool cmdQueue fp 16
    let (fontTexs, fmets) = unzip fontData
        (ftexs, fmipLvls) = unzip fontTexs
    fsamps ← createTextureSamplers dev fmipLvls
    (t2,s2,m2,f2) ← loadFontTexturesF (n+1) fs $ GQData pdev dev cmdPool cmdQueue
    let font = Font fp id (Just (length ftexs)) n
    return (ftexs:t2,fsamps:s2,fmets:m2,font:f2)

loadUtilTextures ∷ GQData → Prog ε σ ([VkImageView],[VkSampler])
loadUtilTextures (GQData pdev dev cmdPool cmdQueue) = do
  -- box texs are for the shell
  let texBoxPath = "dat/tex/box"
  boxTexs ← loadNTexs pdev dev cmdPool cmdQueue texBoxPath
  let (btexs, bsamps) = unzip boxTexs
  return (btexs, bsamps)
