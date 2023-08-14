{-# LANGUAGE DeriveGeneric #-}
-- | data structures for the lua interpreter
module Luau.Data where
-- data for lua interpreter
import qualified Data.Aeson as A
import GHC.Generics

data InputJson   = InputJson   { keySettings ∷ KeySettings }
data KeySettings = KeySettings { keyEscape ∷ String
                               , keyTest   ∷ String }
                               deriving (Generic, Show)
instance A.FromJSON KeySettings
instance A.ToJSON   KeySettings

-- | abstract window
data Window = Window { winTitle  ∷ String
                     , winSize   ∷ (Int,Int)
                     , winPages  ∷ [Page]
                     } deriving (Show, Eq)

-- | each window contains pages, each page contains winElems
data Page = Page { pageTitle  ∷ String
                 } deriving (Show, Eq)

