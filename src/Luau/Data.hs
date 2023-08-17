{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | data structures for the lua interpreter
module Luau.Data where
-- data for lua interpreter
import Data.Aeson
import GHC.Generics

data InputJson   = InputJson   { keySettings ∷ KeySettings } deriving (Generic, Show)
instance FromJSON InputJson where
  parseJSON = withObject "InputJson" $ \v → InputJson
        <$> v .: "keySettings"
instance ToJSON   InputJson where
  toJSON (InputJson keySettings) =
    object ["keySettings" .= keySettings]
  toEncoding (InputJson keySettings) =
    pairs ("keySettings" .= keySettings)

data KeySettings = KeySettings { keyEscape ∷ String
                               , keyTest   ∷ String }
                               deriving (Generic, Show)
instance FromJSON KeySettings where
  parseJSON = withObject "keySettings" $ \v -> KeySettings
        <$> v .: "keyEscape"
        <*> v .: "keyTest"
instance ToJSON   KeySettings where
    toJSON (KeySettings keyEscape keyTest) =
        object ["keyEscape" .= keyEscape, "keyTest" .= keyTest]
    toEncoding (KeySettings keyEscape keyTest) =
        pairs ("keyEscape" .= keyEscape <> "keyTest" .= keyTest)

---- | abstract window
--data Window = Window { winTitle  ∷ String
--                     , winSize   ∷ (Int,Int)
--                     , winPages  ∷ [Page]
--                     } deriving (Show, Eq)
--
---- | each window contains pages, each page contains winElems
--data Page = Page { pageTitle  ∷ String
--                 } deriving (Show, Eq)
--
