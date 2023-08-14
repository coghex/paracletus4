{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
-- | vulkan-specific draw loop, contains calls to GLFW and
--   runs a simple event processor to make changes to state
module Vulk where
-- the main thread is defined
-- TODO: this is the largest file and has a rediculous amount
--       of imports, it needs to be split into multiple files
import Prelude()
import UPrelude
import Control.Concurrent ( forkIO )
import Control.Monad ( forM_, when )
import Control.Monad.State.Class ( gets, modify )
import Data.List ( zip4 )
import GHC.Stack ( HasCallStack )
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain
import Data ( Color(Color), FPS(..) )
import Luau ( luauThread )
import Load.Data ( Dyns(..), Tile(..) )
import Prog ( MonadIO(liftIO), Prog, MonadError(catchError), MonadReader(ask) )
import Prog.Buff ( generateDynData )
import Prog.Data ( State(..), ReloadState(..), LoopControl(..)
                 , TVarName(..), TVarValue(..), ChanName(..) )
import Prog.Event ( processEvents )
import Prog.Input ( inputThread )
import Prog.Foreign ( mallocRes, newArrayRes )
import Prog.Util ( logInfo, logDebug, logError, logExcept, loop, getTime )
import Sign.Data ( TState(..) )
import Sign.Except ( testEx, ExType(ExVulk) )
import Sign.Var
    ( atomically, modifyTVar', newTVar, readTVar, writeTVar )
import Sign.Util ( readTVar', writeTVar', writeChan' )
import Vulk.Buff ( createIndexBuffer, createVertexBuffer )
import Vulk.Command ( createCommandPool )
import Vulk.Calc ( calcVertices )
import Vulk.Desc
    ( createDescriptorPool,
      createDescriptorSets,
      prepareDescriptorSet )
import Vulk.Device
    ( createGraphicsDevice,
      getMaxUsableSampleCount,
      pickPhysicalDevice,
      querySwapchainSupport )
import Vulk.Data
    ( DevQueues(graphicsQueue),
      SwapchainSupportDetails(capabilities),
      TextureData(descSetLayout, descTexInfo, nimages, depthFormat,
                  pipelineLayout),
      Verts(Verts),
      VulkResult(VulkError),
      VulkanLoopData(..) )
import Vulk.Foreign ( runVk )
import Vulk.Instance ( createGLFWVulkanInstance )
import Vulk.Pipeline ( createGraphicsPipeline, createRenderPass )
import Vulk.Pres ( createSurface, createSwapchain )
import Vulk.Shader ( makeShader )
import Vulk.Texture
    ( createColorAttImgView, createDepthAttImgView, createImageView )
import Vulk.Trans
    ( createTransDynBuffers,
      createTransObjBuffers,
      createTransTexBuffers,
      transDynBufferInfo,
      transObjBufferInfo,
      transTexBufferInfo,
      updateTransDyn,
      updateTransObj,
      updateTransTex )
import Vulk.Vertex ( dfLen, vertIADs, vertIBD )
import Vulk.VulkData
    ( GQData(GQData),
      SwapchainInfo(swapImgs, swapImgFormat, swapExtent) )
import Vulk.VulkDraw
    ( createCommandBuffers,
      createFrameFences,
      createFrameSemaphores,
      createFramebuffers,
      drawFrame,
      RenderData(..) )
import Vulk.VulkLoad ( loadVulkanTextures )
import Vulk.VulkGLFW ( glfwWaitEventsMeanwhile, getCurTick, loadLoop
                     , initGLFWWindow, glfwMainLoop )
import qualified Vulk.GLFW as GLFW

runVulk ∷ HasCallStack ⇒ Prog ε σ ()
runVulk = do
    logDebug "beginning paracletus..."
    -- windowsizechanged is completely seperate from all other data
    windowSizeChanged ← liftIO $ atomically $ newTVar True
    -- window loads in at 800 600 by default
    window ← initGLFWWindow 800 600 "paracletus" windowSizeChanged
    -- this is the only glfw window handle that
    -- should be used for write access
    modify $ \s → s { stWindow = Just window }
    -- vulkan specifics
    vulkanInstance ← createGLFWVulkanInstance "paracletus-instance"
    vulkanSurface ← createSurface vulkanInstance window
    -- forks GLFW as parent
    logDebug "forking glfw..."
    glfwWaitEventsMeanwhile $ do
      logDebug "selecting graphics device..."
      -- picks the first suitable device, prioritizing discrete gpu
      (_, pdev)    ← pickPhysicalDevice vulkanInstance
                       (Just vulkanSurface)
      msaaSamples  ← getMaxUsableSampleCount pdev
      (dev,queues) ← createGraphicsDevice pdev vulkanSurface
      -- shader compilation happens at compile time since the
      -- shaders are so simple, this template haskell function
      -- simply inserts that code into vulkan memory
      logDebug "compiling shaders..."
      (shaderVert,shaderFrag) ← makeShader dev
      -- more vulkan specifics
      logDebug "creating semaphores and fences..."
      frameIndexRef ← liftIO $ atomically $ newTVar 0
      renderFinishedSems ← createFrameSemaphores dev
      imageAvailableSems ← createFrameSemaphores dev
      inFlightFences     ← createFrameFences     dev
      commandPool        ← createCommandPool     dev queues
      logDebug "loading system textures..."
      imgIndexPtr ← mallocRes
      let gqdata = GQData pdev dev commandPool (graphicsQueue queues)
      texData ← loadVulkanTextures gqdata []
      -- child threads go here
      logDebug "forking lua interpreter..."
      env ← ask
      _ ← liftIO $ forkIO $ luauThread env
      _ ← liftIO $ forkIO $ inputThread env window
      writeChan' env InputChan TStart
      -- window size change handling
      let beforeSwapchainCreation ∷ Prog ε σ ()
          beforeSwapchainCreation =
            liftIO $ atomically $ modifyTVar' windowSizeChanged
            $ const False
      -- the loop function passes around the LoopControl structure
      loop $ do
          firstTick ← liftIO getCurTick
          scsd ← querySwapchainSupport pdev vulkanSurface
          logDebug "loading swapchain..."
          beforeSwapchainCreation
          recr ← gets stReload
          case recr of
            RSRecreate → do
              -- load textures
              logDebug "recreating vulkan..."
              newTexData ← loadVulkanTextures gqdata []
              modify $ \s → s { stReload = RSNULL
                              , stTick   = Just firstTick }
              let vulkLoopData' = VulkanLoopData {..}
                  vulkLoopData  = vulkLoopData' { texData = newTexData }
              vulkLoop vulkLoopData
            -- when we want to reload but dont need new textures, same
            -- behavior as RSNULL
            _ → do
              let vulkLoopData = VulkanLoopData {..}
              vulkLoop vulkLoopData

-- | this is the main draw loop itself
vulkLoop ∷ VulkanLoopData → Prog ε σ LoopControl
vulkLoop (VulkanLoopData (GQData pdev dev commandPool _) queues scsd0
  window vulkanSurface texData msaaSamples shaderVert shaderFrag
  imgIndexPtr windowSizeChanged frameIndexRef renderFinishedSems
  imageAvailableSems inFlightFences) = do
    -- if the size has changed we need to avoid the KHR capabilities
    -- race condition be resetting the maxExtent values
    sizeChangedInside ← liftIO $ atomically $ readTVar windowSizeChanged
    (scsd,winSize) ← if sizeChangedInside then do
               res ← querySwapchainSupport pdev vulkanSurface
               let windowSize
                     = (fromIntegral $ getField @"width" currentExtent
                       ,fromIntegral $ getField @"height" currentExtent)
                   currentExtent
                     = getField @"currentExtent" $ capabilities res
               return (res,windowSize)
             else do
               let windowSize
                     = (fromIntegral $ getField @"width" currentExtent
                       ,fromIntegral $ getField @"height" currentExtent)
                   currentExtent = getField @"currentExtent"
                                     $ capabilities scsd0
               return (scsd0,windowSize)
    swapInfo ← createSwapchain dev scsd queues vulkanSurface
    let swapchainLen = length (swapImgs swapInfo)
    logDebug "creating object buffers..."
    (transObjMems, transObjBufs)
      ← unzip ⊚ createTransObjBuffers pdev dev swapchainLen
    transObjMemories ← newArrayRes transObjMems
    descriptorBufferInfos ← mapM transObjBufferInfo transObjBufs
    -- TODO: i think its possible to change this number dynamically, it has
    -- an effect on performance since verticies are kept strictly,
    -- but it should be equal to one plus the length of the buffers combined
    let nDynObjs = 20000
    (transDynMems, transDynBufs)
      ← unzip ⊚ createTransDynBuffers pdev dev swapchainLen nDynObjs
    dynDescBufInfos ← mapM (transDynBufferInfo nDynObjs) transDynBufs
    transDynMemories ← newArrayRes transDynMems
    (transTexMems, transTexBufs)
      ← unzip ⊚ createTransTexBuffers pdev dev swapchainLen nDynObjs
    dynTexDescBufInfos ← mapM (transTexBufferInfo nDynObjs) transTexBufs
    transTexMemories ← newArrayRes transTexMems
    -- DESCRIPTOR POOL
    logDebug "creating descriptor pool..."
    descriptorPool ← createDescriptorPool dev swapchainLen (nimages texData)
    descriptorSetLayouts
      ← newArrayRes $ replicate swapchainLen $ descSetLayout texData
    descriptorSets ← createDescriptorSets dev descriptorPool
                       swapchainLen descriptorSetLayouts
    -- different types of tiles would each require
    -- a buf info and dyn info set
    forM_ (zip4 descriptorBufferInfos dynDescBufInfos
      dynTexDescBufInfos descriptorSets)
        $ \(bufInfo, dynBufInfo, dynTexBufInfo, dSet)
          → prepareDescriptorSet dev bufInfo dynBufInfo
              dynTexBufInfo (descTexInfo texData) dSet (nimages texData)
    -- PIPELINE
    logDebug "creating img views..."
    imgViews ← mapM
      (\image → createImageView dev image (swapImgFormat swapInfo)
                 VK_IMAGE_ASPECT_COLOR_BIT 1) (swapImgs swapInfo)
    renderPass ← createRenderPass dev swapInfo
                   (depthFormat texData) msaaSamples
    colorAttImgView ← createColorAttImgView pdev dev commandPool
                        (graphicsQueue queues) (swapImgFormat swapInfo)
                        (swapExtent swapInfo) msaaSamples
    depthAttImgView ← createDepthAttImgView pdev dev commandPool
                        (graphicsQueue queues) (swapExtent swapInfo)
                        msaaSamples
    logDebug "creating pipeline..."
    graphicsPipeline ← createGraphicsPipeline dev swapInfo vertIBD
                         vertIADs [shaderVert, shaderFrag] renderPass
                         (pipelineLayout texData) msaaSamples
    logDebug "creating framebuffers..."
    framebuffers ← createFramebuffers dev renderPass swapInfo imgViews
                     depthAttImgView colorAttImgView
    -- fps counter
    frameCount ← liftIO $ atomically $ newTVar @Int 0
    currentSec ← liftIO $ atomically $ newTVar @Int 0
    -- loop reloads commandBuffer, less of a stutter
    shouldExit ← loadLoop window $ do
      logDebug "generating command buffers..."
      cmdBP ← genCommandBuffs dev pdev commandPool queues graphicsPipeline
                renderPass texData swapInfo framebuffers descriptorSets
      modify $ \s → s { stReload = RSNULL }
      -- main loop runs draw loop and trans functions
      shouldLoad ← glfwMainLoop window $ do
        -- main loop runs draw loop and trans functions
        env ← ask
        dynData' ← readTVar' env DynsTVar
        let Dyns dynData = case dynData' of
                             Nothing          → Dyns []
                             Just (TVDyns d0) → d0
                             Just _           → Dyns []
            nDynsData    = length dynData
            cam = (0,0,-1)
            rdata = RenderData { dev
                               , swapInfo
                               , queues
                               , imgIndexPtr
                               , frameIndexRef
                               , renderFinishedSems
                               , imageAvailableSems
                               , inFlightFences
                               , cmdBuffersPtr = cmdBP
                               , memories = transObjMemories
                               , dynMemories = transDynMemories
                               , texMemories = transTexMemories
                               , memoryMutator = updateTransObj cam dev
                                                   (swapExtent swapInfo)
                               , dynMemoryMutator = updateTransDyn nDynsData
                                                      dynData dev
                                                      (swapExtent swapInfo)
                               , texMemoryMutator = updateTransTex nDynsData
                                                      dynData dev
                                                      (swapExtent swapInfo) }
        liftIO GLFW.pollEvents
        -- khr out of date usually when window is resized
        needRecreation ← if sizeChangedInside
          then do
            _ ← do
              --writeQueue env LoadQueue
              --  $ LoadCmdWindowSize winSize
              logDebug  "vulkan window changing size"
            return True
          else drawFrame rdata `catchError`
            (\err → if testEx err VK_ERROR_OUT_OF_DATE_KHR
              then do
                _ ← logDebug "vulkan khr out of date"
                modify $ \s → s { stReload = RSRecreate }
                return True
              else logExcept VulkError ExVulk "unknown drawFrame error" )
        -- some events must be processed in the parent thread
        processEvents
        -- simple fps counter
        seconds ← getTime
        cur ← liftIO $ atomically $ readTVar currentSec
        if floor seconds ≠ cur then do
          count ← liftIO $ atomically $ readTVar frameCount
          when (cur ≠ 0) $ do
            FPS fpsTarget _ display ← gets stFPS
            modify $ \s → s { stFPS = FPS fpsTarget count display }
          liftIO $ do
            atomically $ writeTVar currentSec (floor seconds)
            atomically $ writeTVar frameCount 0
        else liftIO $ atomically $ modifyTVar' frameCount succ
        -- who knows what this is, just like glWaitForEvents
        runVk $ vkDeviceWaitIdle dev
        -- reload and recreate both exit this inner loop
        stateRel ← gets stReload
        let stateReload = case stateRel of
                            RSNULL → False
                            _      → True
        -- its possible size has changed during the above work so
        -- we check again this solves a vulkan memory overflow
        -- error (possible exploitable bug)
        sizeChanged ← liftIO $ atomically $ readTVar windowSizeChanged
        return $ if needRecreation ∨ stateReload ∨ sizeChanged
                 then AbortLoop else ContinueLoop
      -- outer loop exited with recreate only
      stateRec ← gets stReload
      let stateRecreate = case stateRec of
                            RSRecreate → True
                            _          → False
      sizeChangedOutside ← liftIO
        $ atomically $ readTVar windowSizeChanged
      return $ if shouldLoad then AbortLoop else ContinueLoop
    return $ if shouldExit then AbortLoop else ContinueLoop

-- | command buffers contain all of the work we want to do each frame
--   and are generated on recreation of the swapchain.  this is the
--   function that stutters swapchain recreation since it needs to
--   calculate every vertex/index
genCommandBuffs ∷ VkDevice → VkPhysicalDevice → VkCommandPool
  → DevQueues → VkPipeline → VkRenderPass → TextureData
  → SwapchainInfo → [VkFramebuffer] → [VkDescriptorSet]
  → Prog ε σ (Ptr VkCommandBuffer)
genCommandBuffs dev pdev commandPool queues graphicsPipeline renderPass
  texData swapInfo framebuffers descriptorSets = do
    env ← ask
    -- if there are verts in memory use those,
    -- otherwise draw loading screen
    verts ← readTVar' env VertsTVar
    (verts0,inds0) ← case verts of
      -- without verts in the cache we must regenerate
      Nothing → do
        win ← gets stWindow
        -- loads up a generic background
        (w',h') ← case win of
          Just w0 → liftIO $ GLFW.getWindowSize w0
          Nothing → return (800,600)
        logDebug "generating verticies"
        let res   = calcVertices tiles
            (w,h) = (fromIntegral w'/64.0,fromIntegral h'/64.0)
            tiles = [Tile (0,0) (w,h) (0,0) (1,1) 0, Tile (0,0) (1,1) (0,0) (1,1) 1]
            dyns  = generateDynData tiles
        writeTVar' env DynsTVar $ TVDyns dyns
        writeTVar' env VertsTVar $ TVVerts $ Verts res
        return res
      Just (TVVerts (Verts vs)) → return vs
    vertexBufferNew
      ← createVertexBuffer pdev dev commandPool
        (graphicsQueue queues) verts0
    indexBufferNew
      ← createIndexBuffer pdev dev commandPool
        (graphicsQueue queues) inds0
    createCommandBuffers dev graphicsPipeline commandPool renderPass
      (pipelineLayout texData) swapInfo vertexBufferNew
      (dfLen inds0, indexBufferNew) framebuffers descriptorSets
