{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Damn.Tablumps.TH where

import qualified Data.Attoparsec.ByteString.Char8 as C
import           Data.ByteString                  (ByteString, unpack)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

ary :: Int -> String -> Name -> ExpQ
ary n s con = [e|do
    string s
    C.char '\t'
    Right <$> $(mkApps n con)
    |]

mkApps n con = foldl (\ a b -> [e|$(a) <*> $(b)|]) [e|pure $(conE con)|]
    $ replicate n [e|C.takeWhile (/= '\t') <* C.char '\t'|]

data Lump = Br
          | SlashAbbr

          | Abbr ByteString

          | Dev ByteString ByteString

          | Emote ByteString ByteString ByteString ByteString ByteString

          | Thumb ByteString ByteString ByteString ByteString ByteString ByteString

          | Link ByteString (Maybe ByteString)
          deriving (Lift, Eq, Show)

instance Lift ByteString where
    lift b = [e|pack ws|]
        where ws = unpack b
