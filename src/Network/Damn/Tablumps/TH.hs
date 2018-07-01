{-# LANGUAGE TemplateHaskell #-}

module Network.Damn.Tablumps.TH where

import Data.Attoparsec.ByteString (string)
import qualified Data.Attoparsec.ByteString.Char8 as C
import Data.ByteString (ByteString)
import Language.Haskell.TH
import Prelude.Compat

ary :: Int -> String -> Name -> ExpQ
ary n s con =
    [|do _ <- string s
         _ <- C.char '\t'
         Right <$> $(mkApps)|]
  where
    mkApps =
        foldl (\a b -> [|$(a) <*> $(b)|]) [|pure $(conE con)|] $
        replicate n [|C.takeWhile (/= '\t') <* C.char '\t'|]

-- | Tokens representing tablumps.
--
-- These constructors are defined first in order of arity, then
-- alphabetically.
data Lump
    = A ByteString
        ByteString
    | C_A
    | Abbr ByteString
    | C_Abbr
    | Acro ByteString
    | C_Acro
    | Avatar ByteString
             ByteString
    | B
    | C_B
    | Bcode
    | C_Bcode
    | Br
    | Code
    | C_Code
    | Dev ByteString
          ByteString
    | Embed ByteString
            ByteString
            ByteString
    | C_Embed
    | Emote ByteString
            ByteString
            ByteString
            ByteString
            ByteString
    | I
    | C_I
    | Iframe ByteString
             ByteString
             ByteString
    | C_Iframe
    | Img ByteString
          ByteString
          ByteString
    | Li
    | C_Li
    | Link ByteString
           (Maybe ByteString)
    | Ol
    | C_Ol
    | P
    | C_P
    | S
    | C_S
    | Sub
    | C_Sub
    | Sup
    | C_Sup
    | Thumb ByteString
            ByteString
            ByteString
            ByteString
            ByteString
            ByteString
    | U
    | C_U
    | Ul
    | C_Ul
    deriving (Eq, Show)

{-# ANN module ("HLint: ignore Use camelCase" :: String) #-}
