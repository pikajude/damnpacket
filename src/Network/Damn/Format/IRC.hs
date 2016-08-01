{-# LANGUAGE OverloadedStrings #-}

module Network.Damn.Format.IRC (
    Lines(..), ircFormat
) where

import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import           Data.Monoid
import           Data.String
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           Data.Text.Internal.Builder (toLazyText)
import           Data.Text.Lazy             (toStrict)
import           HTMLEntities.Decoder
import           Network.Damn.Format.Base
import           Network.Damn.Tablumps

newtype Lines = Lines { unLines :: [ByteString] }
                deriving Show

instance IsString Lines where
    fromString = toLines . fromString

toLines s = Lines (B.split 10 s)

instance Monoid Lines where
    mempty = Lines []
    Lines [] `mappend` x = x
    x `mappend` Lines [] = x
    Lines [a] `mappend` Lines [b] = Lines [a <> b]
    Lines xs `mappend` Lines [c] = Lines (init xs ++ [last xs <> c])
    Lines [a] `mappend` Lines (x:xs) = Lines ([a <> x] ++ xs)
    Lines xs `mappend` Lines ys = Lines (init xs ++ [last xs <> head ys] ++ tail ys)

-- | A formatter to make an attempt at converting dAmn tablumps to IRC
-- formatting. This formatter will convert basic styling (bold, italics,
-- etc.) to mIRC colors, convert thumbs and emoticons to their textual
-- representation as they would appear on dAmn, and otherwise converts to
-- HTML tags to preserve data.
--
-- Additionally, @Text@ will be encoded as UTF-8.
--
-- Note that this formatter will generate 'Lines', as newlines (to which
-- @&br\\t@ translates) cannot appear in IRC messages.
ircFormat :: Formatter Lines
ircFormat = either (toLines . encodeUtf8 . htmlDecode) (toLines . ircFormat')

ircFormat' Br                  = "\n"
ircFormat' SlashAbbr           = "</abbr>"
ircFormat' (Abbr x)            = "<abbr title=\"" <> x <> "\">"
ircFormat' (Dev x y)           = x <> y
ircFormat' (Emote x _ _ _ _)   = x
ircFormat' (Thumb x _ _ _ _ _) = ":thumb" <> x <> ":"
ircFormat' (Link x _)          = x
