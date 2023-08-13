{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
-- | command pools are sets of actions to be preformed each frame
module Vulk.Command where
import Prelude()
import UPrelude
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create ( (&*), createVk, set )
import Graphics.Vulkan.Marshal.Create.DataFrame
    ( setDFRef, withDFPtr )
import Numeric.DataFrame ( unScalar )
import Prog ( MonadIO(liftIO), Prog )
import Prog.Foreign ( allocaPeek, allocaPeekDF, newArrayRes )
import Prog.Util ( allocResource, bracket, locally )
import Vulk.Data ( DevQueues(..) )
import Vulk.Foreign ( runVk, withVkPtr )
import Vulk.Sync ( createFence )

-- | allocates resources for a pool of commands
createCommandPool ∷ VkDevice → DevQueues → Prog ε σ VkCommandPool
createCommandPool dev DevQueues{..} =
  allocResource (liftIO ∘ flip (vkDestroyCommandPool dev) VK_NULL)
    $ allocaPeek $ \pPtr → withVkPtr
      ( createVk
        $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
        &* set @"queueFamilyIndex" graphicsFamIdx
      ) $ \ciPtr → runVk $ vkCreateCommandPool dev ciPtr VK_NULL pPtr

-- | runs commands locally in a command buffer then returns the result
--   as alloced in memory
runCommandsOnce ∷ VkDevice → VkCommandPool → VkQueue
  → (VkCommandBuffer → Prog ε σ α) → Prog ε σ α
runCommandsOnce dev commandPool cmdQueue action = do
  let allocInfo = createVk @VkCommandBufferAllocateInfo
        $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
        &* set @"level" VK_COMMAND_BUFFER_LEVEL_PRIMARY
        &* set @"commandPool" commandPool
        &* set @"commandBufferCount" 1
        &* set @"pNext" VK_NULL
  bracket
    (withVkPtr allocInfo $ \aiPtr → allocaPeekDF
      $ runVk ∘ vkAllocateCommandBuffers dev aiPtr)
    (liftIO ∘ flip withDFPtr (vkFreeCommandBuffers dev commandPool 1))
    $ \cmdBufs → do
      let cmdbBI = createVk @VkCommandBufferBeginInfo
            $  set @"sType" VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
            &* set @"flags" VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT
            &* set @"pNext" VK_NULL
          cmdBuf = unScalar cmdBufs
      withVkPtr cmdbBI $ runVk ∘ vkBeginCommandBuffer cmdBuf
      result ← action cmdBuf
      runVk $ vkEndCommandBuffer cmdBuf
      let submitInfo = createVk @VkSubmitInfo
            $  set @"sType" VK_STRUCTURE_TYPE_SUBMIT_INFO
            &* set @"pNext" VK_NULL
            &* set @"waitSemaphoreCount" 0
            &* set @"pWaitSemaphores" VK_NULL
            &* set @"pWaitDstStageMask" VK_NULL
            &* set @"commandBufferCount" 1
            &* setDFRef @"pCommandBuffers" cmdBufs
            &* set @"signalSemaphoreCount" 0
            &* set @"pSignalSemaphores" VK_NULL
      locally $ do
        fence ← createFence dev False
        withVkPtr submitInfo $ \siPtr
          → runVk $ vkQueueSubmit cmdQueue 1 siPtr fence
        fencePtr ← newArrayRes [fence]
        runVk $ vkWaitForFences dev 1 fencePtr VK_TRUE (maxBound ∷ Word64)
      return result
