-- recompilation flag
--{-# OPTIONS_GHC -fforce-recomp #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
-- | uses template haskell to compile shaders before loading at runtime
--   ghc wont recompile the shaders unless you change this file
module Vulk.Shader where
-- shader creation, compiler in Vulk.TH
import Prelude()
import UPrelude ( ($), Monad(return), (∘) )
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Prog ( MonadIO(liftIO), Prog )
import Prog.Foreign ( allocaPeek )
import Prog.Util ( allocResource )
import Vulk.TH ( compileGLSL )
import Vulk.Foreign ( runVk, withVkPtr )

-- | this is the actual shader compilation
makeShader ∷ VkDevice
  → Prog ε σ ( VkPipelineShaderStageCreateInfo
             , VkPipelineShaderStageCreateInfo)
makeShader dev = do
  shaderVert ← createVkShaderStageCI dev
    $(compileGLSL "dat/shd/tile.vert") VK_SHADER_STAGE_VERTEX_BIT
  shaderFrag ← createVkShaderStageCI dev
    $(compileGLSL "dat/shd/tile.frag") VK_SHADER_STAGE_FRAGMENT_BIT
  return (shaderVert, shaderFrag)

-- | we are currently only using one shader, since i
--   dont know how to use more
createVkShaderStageCI ∷ VkDevice → (CSize, Ptr Word32)
  → VkShaderStageFlagBits → Prog ε σ VkPipelineShaderStageCreateInfo
createVkShaderStageCI dev shaderCode stageBit = do
  shaderModule ← createVulkanShaderModule dev shaderCode
  return $ createVk @VkPipelineShaderStageCreateInfo
    $  set @"sType" VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
    &* set @"pNext" VK_NULL
    &* set @"stage" stageBit
    &* set @"module" shaderModule
    &* setStrRef @"pName" "main"

-- | here the code is placed in vulkan memory
createVulkanShaderModule ∷ VkDevice → (CSize, Ptr Word32)
  → Prog ε σ VkShaderModule
createVulkanShaderModule dev (codeSize, codePtr) = allocResource
  (\sm → liftIO $ vkDestroyShaderModule dev sm VK_NULL) $ withVkPtr
    smCreateInfo $ \smciPtr → allocaPeek
    $ runVk ∘ vkCreateShaderModule dev smciPtr VK_NULL
  where smCreateInfo = createVk @VkShaderModuleCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* set @"codeSize" codeSize
          &* set @"pCode" codePtr
          &* set @"flags" VK_ZERO_FLAGS
