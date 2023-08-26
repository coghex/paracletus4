{-# LANGUAGE CPP #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
-- | a vulkan instance is what glfw attaches to, it contains the
--   interface to the most of the vulkan code i beleive
module Vulk.Instance where
import Prelude()
import UPrelude
import Foreign.C.String (peekCString)
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create
import Prog ( MonadIO(liftIO), Prog )
import Prog.Foreign ( allocaPeek )
import Prog.Util ( allocResource, inDev, isDev, logDebug )
import Vulk.Foreign ( runVk, withVkPtr )
import qualified Vulk.GLFW as GLFW
-- | platform dependent vulkan layers
--   TODO: put it windows specific preprocessor code
vkLayerValidation ∷ String
vkLayerValidation = "VK_LAYER_KHRONOS_validation"
-- | vulkan gets instantiated from GLFW
createVulkanInstance ∷ String → String → [CString]
  → [String] → Prog ε σ VkInstance
createVulkanInstance progName engineName extensions layers'
  = allocResource destroyVulkanInstance $ do
    extStrings ← liftIO $ mapM peekCString extensions
    logDebug $ unlines
      $ "[Vulk] enabling instance extensions: " : map (" " ⧺) extStrings
    logDebug $ unlines
      $ "[Vulk] enabling instance layers: " : map (" " ⧺) layers
    withVkPtr iCreateInfo $ \iciPtr → allocaPeek
      $ runVk ∘ vkCreateInstance iciPtr VK_NULL
  where layers = layers' ⧺ [vkLayerValidation | isDev]
        appInfo = createVk @VkApplicationInfo
          $  set @"sType" VK_STRUCTURE_TYPE_APPLICATION_INFO
          &* set @"pNext" VK_NULL
          &* setStrRef @"pApplicationName" progName
          &* set @"applicationVersion" (_VK_MAKE_VERSION 1 0 0)
          &* setStrRef @"pEngineName" engineName
          &* set @"engineVersion" (_VK_MAKE_VERSION 1 0 0)
          &* set @"apiVersion" (_VK_MAKE_VERSION 1 0 68)
        iCreateInfo = createVk @VkInstanceCreateInfo
          $  set @"sType" VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
          &* set @"pNext" VK_NULL
          &* setVkRef @"pApplicationInfo" appInfo
          &* set @"enabledLayerCount" (fromIntegral $ length layers)
          &* setStrListRef @"ppEnabledLayerNames" layers
          &* set @"enabledExtensionCount"
                     (fromIntegral $ length extensions)
          &* setListRef @"ppEnabledExtensionNames" extensions

-- | clear up vulkan code
destroyVulkanInstance ∷ VkInstance → Prog ε σ ()
destroyVulkanInstance vkInstance = liftIO
  (vkDestroyInstance vkInstance VK_NULL) ≫ inDev
    (logDebug "[Vulk] destroyed vkInstance")
-- | vulkan specific glfw function
createGLFWVulkanInstance ∷ String → Prog ε σ VkInstance
createGLFWVulkanInstance progName = do
  glfwReqExts ← liftIO GLFW.getRequiredInstanceExtensions
  createVulkanInstance progName "paracletus" glfwReqExts []
