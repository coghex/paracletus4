{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- | data structure and some simple functions for an abstract vertex
module Vulk.Vertex where
import Prelude()
import UPrelude
import qualified Control.Monad.ST as ST
import Data.Maybe ( fromMaybe )
import GHC.Generics (Generic)
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create ( (&*), createVk, set )
import Graphics.Vulkan.Marshal.Create.DataFrame()
import Numeric.DataFrame
import qualified Numeric.DataFrame.ST as ST
import Numeric.Dimensions
    ( Dimensions(dims), KnownDimType, dimVal, All, BoundedDims )

-- | a vertex's data is processed into dataframes in Vulk.Calc
data Vertex = Vertex { pos      ∷ Vec3f
                     , color    ∷ Vec4f
                     , texCoord ∷ Vec3f
                     , move     ∷ Vec3f
                     } deriving (Eq, Ord, Show, Generic)
instance PrimBytes Vertex

-- | this is to pass a compile time check of the dataframe library
atLeastThree ∷ (All KnownDimType ns, BoundedDims ns)
  ⇒ DataFrame t (n ': ns) → DataFrame t (XN 3 ': ns)
atLeastThree = fromMaybe (error "not enough vertex points")
             ∘ constrainDF

-- | word32 length of a dataframe
dfLen ∷ DataFrame t (xns ∷ [XNat]) → Word32
dfLen (XFrame (_ ∷ DataFrame t ns)) = case dims @ns of
  n :* _ → fromIntegral $ dimVal n
  U      → 1

-- | input binding description, new bindings would go here
vertIBD ∷ VkVertexInputBindingDescription
vertIBD = createVk
  $  set @"binding"   0
  &* set @"stride"    (bSizeOf @Vertex undefined)
  &* set @"inputRate" VK_VERTEX_INPUT_RATE_VERTEX
-- | input attribution description of eacch descriptor, these
--   are the locations, not the bindings, int the vert code
vertIADs ∷ Vector VkVertexInputAttributeDescription 4
vertIADs = ST.runST $ do
  mv ← ST.newPinnedDataFrame
  ST.writeDataFrame mv (0 :* Empty) ∘ scalar $ createVk
    $  set @"location" 0
    &* set @"binding"  0
    &* set @"format"   VK_FORMAT_R32G32B32_SFLOAT
    &* set @"offset"   (bFieldOffsetOf @"pos" @Vertex undefined)
  ST.writeDataFrame mv (1 :* Empty) ∘ scalar $ createVk
    $  set @"location" 1
    &* set @"binding"  0
    &* set @"format"   VK_FORMAT_R32G32B32A32_SFLOAT
    &* set @"offset"   (bFieldOffsetOf @"color" @Vertex undefined)
  ST.writeDataFrame mv (2 :* Empty) ∘ scalar $ createVk
    $  set @"location" 2
    &* set @"binding"  0
    &* set @"format"   VK_FORMAT_R32G32B32_SFLOAT
    &* set @"offset"   (bFieldOffsetOf @"texCoord" @Vertex undefined)
  ST.writeDataFrame mv (3 :* Empty) ∘ scalar $ createVk
    $  set @"location" 3
    &* set @"binding"  0
    &* set @"format"   VK_FORMAT_R32G32B32_SFLOAT
    &* set @"offset"   (bFieldOffsetOf @"move" @Vertex undefined)
  ST.unsafeFreezeDataFrame mv
