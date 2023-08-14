-- | this is an interface module, that exports the original GLFW with
--   some extra structures and functions attached
module Vulk.GLFW
  ( module Graphics.UI.GLFW
  , KeyLayout (..)
  , keyCheck
  , numCheck
  , calcInpKey
  , mousebutt1
  , mousebutt2
  , mousebutt3
  ) where
-- the input is defined for glfw, since
-- the library is just one giant unified
-- C library, we dont want to import
-- everything all at once, and this way
-- we can abstract to other window managers
import Prelude()
import UPrelude
import Data.Char (toUpper,toLower)
import qualified Data as P
import Graphics.UI.GLFW

-- | TODO: remove all instances of this
data KeyLayout = KeyLayout
  { klEsc ∷ String
  , klRet ∷ String
  , klDel ∷ String
  , klSpc ∷ String
  , klTab ∷ String
  , klUp  ∷ String
  , klLft ∷ String
  , klDwn ∷ String
  , klRgt ∷ String
  , klSh  ∷ String } deriving (Show, Eq)

-- | mousebutton synonyms
mousebutt1 ∷ MouseButton
mousebutt1 = MouseButton'1
mousebutt2 ∷ MouseButton
mousebutt2 = MouseButton'2
mousebutt3 ∷ MouseButton
mousebutt3 = MouseButton'3
--mousebutt4 ∷ GLFW.MouseButton
--mousebutt4 = GLFW.MouseButton'4
--mousebutt5 ∷ GLFW.MouseButton
--mousebutt5 = GLFW.MouseButton'5
--mousebutt6 ∷ GLFW.MouseButton
--mousebutt6 = GLFW.MouseButton'6
--mousebutt7 ∷ GLFW.MouseButton
--mousebutt7 = GLFW.MouseButton'7
--mousebutt8 ∷ GLFW.MouseButton
--mousebutt8 = GLFW.MouseButton'8

-- | translates keys from strings for ease of use
-- | TODO: delete this
getGLFWKey ∷ String → Key
getGLFWKey "ESC" = Key'Escape
getGLFWKey "RET" = Key'Enter
getGLFWKey "DEL" = Key'Backspace
getGLFWKey "SPC" = Key'Space
getGLFWKey "TAB" = Key'Tab
getGLFWKey "LFT" = Key'Left
getGLFWKey "RGT" = Key'Right
getGLFWKey "UPP" = Key'Up
getGLFWKey "DWN" = Key'Down
getGLFWKey "SH"  = Key'GraveAccent
getGLFWKey "C"   = Key'C
getGLFWKey "E"   = Key'E
getGLFWKey "R"   = Key'R
getGLFWKey "H"   = Key'H
getGLFWKey "J"   = Key'J
getGLFWKey "K"   = Key'K
getGLFWKey "L"   = Key'L
getGLFWKey "W"   = Key'W
getGLFWKey "A"   = Key'A
getGLFWKey "S"   = Key'S
getGLFWKey "D"   = Key'D
getGLFWKey _     = Key'Unknown

-- | TODO: delete this
keyCheck ∷ Bool → KeyLayout → Key → String → Bool
keyCheck cap keyLayout k str
  | cap       = False
  | otherwise = (k ≡ (getGLFWKey nk))
  where nk = applyKeyLayout keyLayout str

-- | glfw keys to integers
numCheck ∷ Key → Int
numCheck Key'0 = 0
numCheck Key'1 = 1
numCheck Key'2 = 2
numCheck Key'3 = 3
numCheck Key'4 = 4
numCheck Key'5 = 5
numCheck Key'6 = 6
numCheck Key'7 = 7
numCheck Key'8 = 8
numCheck Key'9 = 9
numCheck _     = -1

-- | TODO: delete this 
applyKeyLayout ∷ KeyLayout → String → String
applyKeyLayout keyLayout "ESC" = klEsc keyLayout
applyKeyLayout keyLayout "RET" = klRet keyLayout
applyKeyLayout keyLayout "DEL" = klDel keyLayout
applyKeyLayout keyLayout "SPC" = klSpc keyLayout
applyKeyLayout keyLayout "TAB" = klTab keyLayout
applyKeyLayout keyLayout "LFT" = klLft keyLayout
applyKeyLayout keyLayout "RGT" = klRgt keyLayout
applyKeyLayout keyLayout "UP"  = klUp  keyLayout
applyKeyLayout keyLayout "DWN" = klDwn keyLayout
applyKeyLayout keyLayout "SH"  = klSh  keyLayout
applyKeyLayout _         "LFA" = "LFT"
applyKeyLayout _         "RTA" = "RGT"
applyKeyLayout _         "UPA" = "UPP"
applyKeyLayout _         "DNA" = "DWN"
applyKeyLayout _         "H"   = "H"
applyKeyLayout _         "J"   = "J"
applyKeyLayout _         "K"   = "K"
applyKeyLayout _         "L"   = "L"
applyKeyLayout _         "C"   = "C"
applyKeyLayout _         "A"   = "A"
applyKeyLayout _         "R"   = "R"
applyKeyLayout _         "E"   = "E"
applyKeyLayout _         _     = "NULL"

-- | convert a known key into possible a string
getKeyStr ∷ Key → IO (Maybe String)
getKeyStr k = getKeyName k 0

-- | takes the input key and applys shift if neccesary
calcInpKey ∷ Key → ModifierKeys → IO String
calcInpKey k mk = do
  inp ← getKeyStr k
  case inp of
    Just str → return $ applyMod mk str
    Nothing  → return ""

-- | takes a string and applys uppercase on glfw shift
applyMod ∷ ModifierKeys → String → String
applyMod mk str = if modifierKeysShift mk
  then map upcase str else map downcase str

-- | different keys have different uppercase results, toUpper from
--   Data.Char can handle all of the alphabet chars
upcase ∷ Char → Char
upcase '1'  = '!'
upcase '2'  = '@'
upcase '3'  = '#'
upcase '4'  = '$'
upcase '5'  = '%'
upcase '6'  = '^'
upcase '7'  = '&'
upcase '8'  = '*'
upcase '9'  = '('
upcase '0'  = ')'
upcase '-'  = '_'
upcase '='  = '+'
upcase '['  = '{'
upcase ']'  = '}'
upcase '\\' = '|'
upcase ';'  = ':'
upcase '\'' = '"'
upcase ','  = '<'
upcase '.'  = '>'
upcase '/'  = '?'
upcase '`'  = '~'
upcase c    = toUpper c
-- | i dont know why one would want this but here it is
--   Data.Char can handle all of the alphabet chars
downcase ∷ Char → Char
downcase '!' = '1'
downcase '@' = '2'
downcase '#' = '3'
downcase '$' = '4'
downcase '%' = '5'
downcase '^' = '6'
downcase '&' = '7'
downcase '*' = '8'
downcase '(' = '9'
downcase ')' = '0'
downcase '_' = '-'
downcase '+' = '='
downcase '{' = '['
downcase '}' = ']'
downcase '|' = '\\'
downcase ':' = ';'
downcase '"' = '\''
downcase '<' = ','
downcase '>' = '.'
downcase '?' = '/'
downcase '~' = '`'
downcase c  = toLower c


--findKey ∷ Key → P.Key
--findKey Key'A            = P.KeyA
--findKey Key'B            = P.KeyB
--findKey Key'C            = P.KeyC
--findKey Key'D            = P.KeyD
--findKey Key'E            = P.KeyE
--findKey Key'F            = P.KeyF
--findKey Key'G            = P.KeyG
--findKey Key'H            = P.KeyH
--findKey Key'I            = P.KeyI
--findKey Key'J            = P.KeyJ
--findKey Key'K            = P.KeyK
--findKey Key'L            = P.KeyL
--findKey Key'M            = P.KeyM
--findKey Key'N            = P.KeyN
--findKey Key'O            = P.KeyO
--findKey Key'P            = P.KeyP
--findKey Key'Q            = P.KeyQ
--findKey Key'R            = P.KeyR
--findKey Key'S            = P.KeyS
--findKey Key'T            = P.KeyT
--findKey Key'U            = P.KeyU
--findKey Key'V            = P.KeyV
--findKey Key'W            = P.KeyW
--findKey Key'X            = P.KeyX
--findKey Key'Y            = P.KeyY
--findKey Key'Z            = P.KeyZ
--findKey Key'0            = P.Key0
--findKey Key'1            = P.Key1
--findKey Key'2            = P.Key2
--findKey Key'3            = P.Key3
--findKey Key'4            = P.Key4
--findKey Key'5            = P.Key5
--findKey Key'6            = P.Key6
--findKey Key'7            = P.Key7
--findKey Key'8            = P.Key8
--findKey Key'9            = P.Key9
--findKey Key'Space        = P.KeySpace
--findKey Key'Apostrophe   = P.KeyQuote
--findKey Key'Comma        = P.KeyComma
--findKey Key'Minus        = P.KeyMinus
--findKey Key'Period       = P.KeyPeriod
--findKey Key'Slash        = P.KeySlash
--findKey Key'Semicolon    = P.KeyColon
--findKey Key'Equal        = P.KeyEqual
--findKey Key'LeftBracket  = P.KeyBrackLeft
--findKey Key'RightBracket = P.KeyBrackRight
--findKey Key'Backslash    = P.KeyPipe
--findKey Key'GraveAccent  = P.KeyTilde
--findKey Key'Escape       = P.KeyEscape
--findKey Key'Enter        = P.KeyReturn
--findKey Key'Tab          = P.KeyTab
--findKey Key'Backspace    = P.KeyBackspace
--findKey Key'Insert       = P.KeyInsert
--findKey Key'Delete       = P.KeyDelete
--findKey Key'Up           = P.KeyUp
--findKey Key'Down         = P.KeyDown
--findKey Key'Right        = P.KeyRight
--findKey Key'Left         = P.KeyLeft
--findKey Key'PageUp       = P.KeyPageUp
--findKey Key'PageDown     = P.KeyPageDown
--findKey Key'Home         = P.KeyHome
--findKey Key'End          = P.KeyEnd
--findKey Key'CapsLock     = P.KeyCaps
--findKey Key'ScrollLock   = P.KeyScrLk
--findKey Key'NumLock      = P.KeyNumLk
--findKey Key'PrintScreen  = P.KeyPrtSc
--findKey Key'Pause        = P.KeyPause
--findKey Key'F1           = P.KeyF1
--findKey Key'F2           = P.KeyF2
--findKey Key'F3           = P.KeyF3
--findKey Key'F4           = P.KeyF4
--findKey Key'F5           = P.KeyF5
--findKey Key'F6           = P.KeyF6
--findKey Key'F7           = P.KeyF7
--findKey Key'F8           = P.KeyF8
--findKey Key'F9           = P.KeyF9
--findKey Key'F10          = P.KeyF10
--findKey Key'F11          = P.KeyF11
--findKey Key'F12          = P.KeyF12
--findKey Key'F13          = P.KeyF13
--findKey Key'F14          = P.KeyF14
--findKey Key'F15          = P.KeyF15
--findKey Key'F16          = P.KeyF16
--findKey Key'F17          = P.KeyF17
--findKey Key'F18          = P.KeyF18
--findKey Key'F19          = P.KeyF19
--findKey Key'F20          = P.KeyF20
--findKey Key'F21          = P.KeyF21
--findKey Key'F22          = P.KeyF22
--findKey Key'F23          = P.KeyF23
--findKey Key'F24          = P.KeyF24
--findKey Key'F25          = P.KeyF25
--findKey Key'Pad0         = P.KeyNum0
--findKey Key'Pad1         = P.KeyNum1
--findKey Key'Pad2         = P.KeyNum2
--findKey Key'Pad3         = P.KeyNum3
--findKey Key'Pad4         = P.KeyNum4
--findKey Key'Pad5         = P.KeyNum5
--findKey Key'Pad6         = P.KeyNum6
--findKey Key'Pad7         = P.KeyNum7
--findKey Key'Pad8         = P.KeyNum8
--findKey Key'Pad9         = P.KeyNum9
--findKey Key'PadDecimal   = P.KeyNumDot
--findKey Key'PadDivide    = P.KeyNumDiv
--findKey Key'PadMultiply  = P.KeyNumMul
--findKey Key'PadSubtract  = P.KeyNumSub
--findKey Key'PadAdd       = P.KeyNumAdd
--findKey Key'PadEnter     = P.KeyNumRet
--findKey Key'PadEqual     = P.KeyNumEql
--findKey Key'LeftShift    = P.KeyLShift
--findKey Key'LeftControl  = P.KeyLCtrl
--findKey Key'LeftAlt      = P.KeyLAlt
--findKey Key'LeftSuper    = P.KeyLSuper
--findKey Key'RightShift   = P.KeyRShift
--findKey Key'RightControl = P.KeyRCtrl
--findKey Key'RightAlt     = P.KeyRAlt
--findKey Key'RightSuper   = P.KeyRSuper
--findKey Key'Menu         = P.KeyMenu
--findKey Key'World1       = P.KeyNULL
--findKey Key'World2       = P.KeyNULL
--findKey Key'Unknown      = P.KeyNULL
