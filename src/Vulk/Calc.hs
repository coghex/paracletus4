{-# LANGUAGE Strict #-}
{-# LANGUAGE KindSignatures #-}
-- | functions used by the load thread to convert a list of tiles into
--   a tuple of dataframes consisting of verticies and indicies ready to
--   go straight into a command buffer. requires much computational work
module Vulk.Calc
where
-- translations from lua state to draw
-- state are defined, should be run
-- outside of parent thread
import Prelude()
import UPrelude
import Graphics.Vulkan.Core_1_0 ( Word32 )
import Numeric.DataFrame
import Data ( Color(..) )
import Load.Data ( Tile(..), TilePos(..), TileTex(..) )
import Vulk.Atlas ( indexAtlas )
import Vulk.Vertex
    ( atLeastThree, Vertex(..) )

-- | determines dataframes from drawstate
calcVertices ∷ [Tile]
  → (DataFrame Vertex '[XN 0], DataFrame Word32 '[XN 3])
calcVertices ts = (vertices ts, indices ts)

-- | a simple base matrix to work off of, each square has four vertecies.
--   and  is drawn by vulkan as a set of two triangles.
-- TODO: great speedups could happen if we learn how to use this library
-- and build the dataframes directly from the tiles instead of making
-- a base identity value and altering it with each transformation
vertsqs ∷ [DataFrame Vertex ('[] ∷ [Nat])]
vertsqs
  = [ S $ Vertex (vec3 (-1) (-1) 0) (vec4 1 0 0 1)
                 (vec3 0 1 0.1) (vec3 0 0 0)
    , S $ Vertex (vec3   1  (-1) 0) (vec4 0 1 0 1)
                 (vec3 1 1 0.1) (vec3 0 0 0)
    , S $ Vertex (vec3   1    1  0) (vec4 0 0 1 1)
                 (vec3 1 0 0.1) (vec3 0 0 0)
    , S $ Vertex (vec3 (-1)   1  0) (vec4 1 1 1 1)
                 (vec3 0 0 0.1) (vec3 0 0 0) ]
-- | combines all Tiles into a dataframe
vertices ∷ [Tile] → DataFrame Vertex '[XN 0]
vertices ts = fromList $ combineVertices (1∷Int) ts
  where combineVertices _    [] = []
        combineVertices nDyn
          ((Tile _
          (TileTex _ (sx,sy) _)):tts)
            = withColor (withMove (+ vec3 1 dyn moves')
              (withTC (indexAtlas 0 0 sx sy) (withTC (+ vec3 0 0 t)
              (withPos (+ vec4 x0 y0 0 0)
              (withScale (* vec3 xscale yscale 1) vertsqs)))))
                ⧺ combineVertices (nDyn + 1) tts where
          (x0,y0) = (0,0)
          (xscale, yscale)  = (1,1)
          t = 0
          dyn = fromIntegral nDyn
          moves' = 1
          withPos f = map (\(S v) → S v { pos
            = fromHom ∘ f ∘ toHomPoint $ pos v })
          withTC f = map (\(S v) → S v { texCoord = f $ texCoord v })
          withScale f = map (\(S v) → S v { pos = f $ pos v })
          withMove f = map (\(S v) → S v { move = f $ move v })
          withColor = map (\(S v) → S v { color = vec4 1 1 1 1 })

-- | vulkan will draw the dataframe for each tile in this order, six
--   points for the 6 verticies in two triangles laid together squarely
indices ∷ [Tile] → DataFrame Word32 '[XN 3]
indices tiles = atLeastThree $ fromList $ combineIndices tiles
--   this function is suprisingly slow
--combineIndices ∷ ∀ a. (Num a) ⇒ [Tile] → [a]
--combineIndices []           = []
--combineIndices (_:tiles) = oneRectIndices ⧺ map (+4) (combineIndices tiles)
--  where oneRectIndices = [0,3,2,2,1,0]
--   this one speeds everything up dramatically
combineIndices ∷ ∀ a. (Num a) ⇒ [Tile] → [a]
combineIndices tiles = indexList 0 (length tiles)
indexList ∷ ∀ a. (Num a) ⇒ a → Int → [a]
indexList _ 0 = []
indexList i n = a:b:c:d:e:f:indexList (i+4) (n-1)
  where (a,b,c,d,e,f) = (i,i+3,i+2,i+2,i+1,i)
