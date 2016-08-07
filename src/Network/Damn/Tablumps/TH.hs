{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}

#define ARG ByteString

module Network.Damn.Tablumps.TH where

import qualified Data.Attoparsec.ByteString.Char8 as C
import           Data.ByteString                  (ByteString, unpack)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

ary :: Int -> String -> Name -> ExpQ
ary n s con = [e|do
    _ <- string s
    _ <- C.char '\t'
    Right <$> $(mkApps)
    |]
    where
        mkApps = foldl (\ a b -> [e|$(a) <*> $(b)|]) [e|pure $(conE con)|]
            $ replicate n [e|C.takeWhile (/= '\t') <* C.char '\t'|]

-- | Tokens representing tablumps.
--
-- These constructors are defined first in order of arity, then
-- alphabetically.
data Lump = A ARG ARG
          | C_A
          | Abbr ARG
          | C_Abbr
          | Acro ARG
          | C_Acro
          | Avatar ARG ARG
          | B
          | C_B
          | Bcode
          | C_Bcode
          | Br
          | Code
          | C_Code
          | Dev ARG ARG
          | Embed ARG ARG ARG
          | C_Embed
          | Emote ARG ARG ARG ARG ARG
          | I
          | C_I
          | Iframe ARG ARG ARG
          | C_Iframe
          | Img ARG ARG ARG
          | Li
          | C_Li
          | Link ARG (Maybe ARG)
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
          | Thumb ARG ARG ARG ARG ARG ARG
          | U
          | C_U
          | Ul
          | C_Ul
          deriving (Lift, Eq, Show)

instance Lift ByteString where
    lift b = [e|pack ws|]
        where ws = unpack b
