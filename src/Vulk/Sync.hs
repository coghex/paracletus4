{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
-- | vulkan semaphores and fences
module Vulk.Sync where
import Prelude()
import UPrelude ( ($), Bool, flip, (∘) )
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create ( (&*), createVk, set )
import Prog ( MonadIO(liftIO), Prog )
import Prog.Foreign ( allocaPeek )
import Prog.Util ( allocResource )
import Vulk.Foreign ( runVk, withVkPtr )

-- | allocatess a semaphore for thread safe vulkaning
createSemaphore ∷ VkDevice → Prog ε σ VkSemaphore
createSemaphore dev = allocResource
  (liftIO ∘ flip (vkDestroySemaphore dev) VK_NULL)
    $ allocaPeek $ \sPtr → withVkPtr
      (createVk
        $  set @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" VK_ZERO_FLAGS
      ) $ \ciPtr → runVk $ vkCreateSemaphore dev ciPtr VK_NULL sPtr
-- | allocatess a fence for thread safe vulkaning
createFence ∷ VkDevice → Bool → Prog ε σ VkFence
createFence dev signaled = allocResource
  (liftIO ∘ flip (vkDestroyFence dev) VK_NULL)
    $ allocaPeek $ \sPtr → withVkPtr
      (createVk
        $  set @"sType" VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
        &* set @"pNext" VK_NULL
        &* set @"flags" (if signaled
                         then VK_FENCE_CREATE_SIGNALED_BIT
                         else VK_ZERO_FLAGS)
      ) $ \ciPtr → runVk $ vkCreateFence dev ciPtr VK_NULL sPtr
