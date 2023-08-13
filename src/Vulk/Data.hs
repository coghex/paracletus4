{-# LANGUAGE Strict #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures #-}
-- | structures used by the main draw loop to pass around vulkan data
module Vulk.Data where
-- some general data structures are defined
import Prelude()
import UPrelude
import Numeric.DataFrame ( DataFrame, XN )
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_surface
import Sign.Var ( TVar )
import Vulk.Vertex ( Vertex )
import Vulk.VulkData ( GQData )
import qualified Vulk.GLFW as GLFW

-- | possible results of a paracletus evaluation
data VulkResult = VulkSuccess | VulkError | GLFWError deriving (Show, Eq)

-- | fundamental vertex/index data frame, hlint is sugesting newtype
--   which is probably a good idea since newtypes are strict
newtype Verts = Verts ( DataFrame Vertex '[XN 0]
                      , DataFrame Word32 '[XN 3])

-- | all the data required for a set of textures
data TextureData = TextureData
         { descSetLayout  ∷ VkDescriptorSetLayout
         , pipelineLayout ∷ VkPipelineLayout
         , nimages        ∷ Int
         , descTexInfo    ∷ [VkDescriptorImageInfo]
         , depthFormat    ∷ VkFormat }

-- | the main data for the vulkan loop
data VulkanLoopData = VulkanLoopData
         { gqdata             ∷ GQData
         , queues             ∷ DevQueues
         , scsd               ∷ SwapchainSupportDetails
         , window             ∷ GLFW.Window
         , vulkanSurface      ∷ VkSurfaceKHR
         , texData            ∷ TextureData
         , msaaSamples        ∷ VkSampleCountFlagBits
         , shaderVert         ∷ VkPipelineShaderStageCreateInfo
         , shaderFrag         ∷ VkPipelineShaderStageCreateInfo
         , imgIndexPtr        ∷ Ptr Word32
         , windowSizeChanged  ∷ TVar Bool
         , frameIndexRef      ∷ TVar Int
         , renderFinishedSems ∷ Ptr VkSemaphore
         , imageAvailableSems ∷ Ptr VkSemaphore
         , inFlightFences     ∷ Ptr VkFence }

-- | we are only using one device, so queues are
--   only relevant to pass data around
data DevQueues = DevQueues { graphicsQueue  ∷ VkQueue
                           , presentQueue   ∷ VkQueue
                           , qFamIndices    ∷ Ptr Word32
                           , graphicsFamIdx ∷ Word32
                           , presentFamIdx  ∷ Word32
                           } deriving (Eq, Show)

-- | a structure of the capabilities of the device in question
data SwapchainSupportDetails = SwapchainSupportDetails
  { capabilities ∷ VkSurfaceCapabilitiesKHR
  , formats      ∷ [VkSurfaceFormatKHR]
  , presentModes ∷ [VkPresentModeKHR]
  } deriving (Eq, Show)

