{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | data structures for the lua interpreter
module Luau.Data where
-- data for lua interpreter
import Data.Aeson
import qualified Data.Map as M
import GHC.Generics
import Data ( ID(..) )
import Load.Data ( DebugLevel(..) )
import qualified Vulk.GLFW as GLFW

-- | input settings as laid out in json file
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
                               , keyTest   ∷ String
                               , keyShell  ∷ String }
                               deriving (Generic, Show)
instance FromJSON KeySettings where
  parseJSON = withObject "keySettings" $ \v -> KeySettings
        <$> v .: "keyEscape"
        <*> v .: "keyTest"
        <*> v .: "keyShell"
instance ToJSON   KeySettings where
    toJSON (KeySettings keyEscape keyTest keyShell) =
        object ["keyEscape" .= keyEscape, "keyTest" .= keyTest, "keyShell" .= keyShell]
    toEncoding (KeySettings keyEscape keyTest keyShell) =
        pairs ("keyEscape" .= keyEscape <> "keyTest" .= keyTest <> "keyShell" .= keyShell)

-- | possible commands to send to the luau thread
data LuauCmd = LuauCmdReadUD | LuauCmdNULL deriving (Show,Eq)
-- | possible results of a luau thread computation
data LuauResult = LuauResultSuccess | LuauResultError String deriving (Show, Eq)

-- | possible commands to send the shell
data ShellCmd = ShToggle
              | ShKey GLFW.Key GLFW.ModifierKeys
              | ShEcho String | ShHistory | ShClear | ShExit
              | ShNULL deriving (Show, Eq)

-- | collection of user made variables
newtype UserData = UserData (M.Map ID UserVar) deriving (Show, Eq)
data UserVar = UVInt Int | UVBool Bool | UVWindow GLFW.Window
             | UVDebugFlags [DebugLevel]
             | UVNULL deriving (Show, Eq)
