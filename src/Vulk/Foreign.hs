{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedTuples #-}
-- | low level memory functions for vulkan data
module Vulk.Foreign where
-- vulkan specific pointer functions
import Prelude()
import UPrelude
import Control.Monad (when)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import qualified Foreign.Marshal.Array as Foreign
import Foreign.Storable (Storable)
import qualified Foreign.Storable as Storable
import GHC.Stack ( HasCallStack, prettyCallStack, callStack )
import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0 ( VkResult(VK_SUCCESS) )
import Sign.Except ( ProgExcept(ProgExcept), ExType(ExVulk) )
import Prog
    ( MonadIO(liftIO),
      MonadError(throwError),
      MonadState(state),
      Prog(..),
      Prog' )
import Prog.Data ( State(stStatus) )
import Prog.Foreign ( alloca, allocaArray, liftIOWith, touch )

-- | runs io vulkan command,
--   throwing unique exception
runVk ∷ HasCallStack ⇒ IO VkResult → Prog ε σ ()
runVk action = do
  r ← liftIO action
  let ret = ProgExcept (Just r) ExVulk
              $ "vulkan command returned error: "
              ⧺ show r ⧺ "\n\n" ⧺ prettyCallStack callStack
  state $ \s → ((), s { stStatus = ret })
  when (r < VK_SUCCESS) $ throwError ret
{-# INLINE runVk #-}
-- | executes function over memory
withVkPtr ∷ VulkanMarshal α ⇒ α → (Ptr α → Prog' ε β) → Prog ε σ β
withVkPtr x = liftIOWith (withPtr x)
{-# INLINE withVkPtr #-}
-- | executes function over memory then returns the new memory
allocaPeekVk ∷ VulkanMarshal α ⇒ (Ptr α → Prog ε () ()) → Prog ε σ α
allocaPeekVk pf = Prog $ \e s c → do
  locVar ← liftIO newEmptyMVar
  a ← newVkData (\ptr → unProg (pf ptr) e s (putMVar locVar))
  takeMVar locVar ⌦ c ∘ (a ⚟)
-- | sometimes vulkan commands want lists as arguments
asListVk ∷ Storable γ ⇒ (Ptr Word32 → Ptr γ
  → Prog ε (Either ProgExcept [γ]) ()) → Prog ε σ [γ]
asListVk action = alloca $ \counterPtr → do
  action counterPtr VK_NULL_HANDLE
  counter ← liftIO $ fromIntegral ⊚ Storable.peek counterPtr
  if counter ≤ 0 then pure [] else allocaArray counter $ \valPtr → do
    action counterPtr valPtr
    liftIO $ Foreign.peekArray counter valPtr
-- | like withArrayLen from base but for vulkan data
withVkArrayLen ∷ (Storable α, VulkanMarshal α)
  ⇒ [α] → (Word32 → Ptr α → IO β) → IO β
withVkArrayLen xs pf = do
  ret ← Foreign.withArrayLen xs (pf ∘ fromIntegral)
  touch xs
  return ret
{-# INLINE withVkArrayLen #-}
-- | TODO: look into why i have two of the identical function
withArrayLen ∷ (Storable α, VulkanMarshal α)
  ⇒ [α] → (Word32 → Ptr α → IO β) → IO β
withArrayLen xs pf = do
  ret ← Foreign.withArrayLen xs (pf ∘ fromIntegral)
  touch xs
  return ret
{-# INLINE withArrayLen #-}
