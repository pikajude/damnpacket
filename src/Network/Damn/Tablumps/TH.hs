{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}

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
data Lump = B -- ^ @\<b\>@
          | Br -- ^ @\<br/\>@
          | I -- ^ @\<i\>@
          | U -- ^ @\<u\>@

          | CloseAbbr -- ^ @\</abbr\>@
          | CloseB -- ^ @\</b\>@
          | CloseI -- ^ @\</i\>@
          | CloseU -- ^ @\</u\>@

          | Abbr ByteString -- ^ @\<abbr title="$1"\>@

          | Dev ByteString ByteString -- ^ @$1\<a href="$2.deviantart.com"\>$2\</a\>@

          | -- | @\<img alt="$1" width="$2" height="$3" title="$4" src="http://e.deviantart.com/emoticons/$5" /\>@
            Emote ByteString ByteString ByteString ByteString ByteString

          | Thumb ByteString ByteString ByteString ByteString ByteString ByteString -- ^ @:thumb$1:@

          | Link ByteString (Maybe ByteString) -- ^ @\<a href="$1" title="$1"\>$2 or "[link]"\</a\>@
          deriving (Lift, Eq, Show)

instance Lift ByteString where
    lift b = [e|pack ws|]
        where ws = unpack b
