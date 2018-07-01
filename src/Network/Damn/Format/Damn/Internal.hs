{-# LANGUAGE OverloadedStrings #-}

module Network.Damn.Format.Damn.Internal
    ( textToBytes
    , damnFormat'
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LB (toStrict)
import Data.Char
import Data.Monoid.Compat
import Data.Text (Text)
import qualified Data.Text
import Network.Damn.Tablumps.TH
import Prelude.Compat

textToBytes :: Text -> ByteString
textToBytes =
    LB.toStrict . toLazyByteString . Data.Text.foldr (\c b -> maybeEscape c <> b) ""
  where
    maybeEscape c
        | ord c <= 127 = word8 (fromIntegral $ ord c)
        | otherwise = "&#x" <> word32Hex (fromIntegral $ ord c) <> ";"

damnFormat' :: Lump -> ByteString
damnFormat' (A x y) = "<a href=\"" <> x <> "\" title=\"" <> y <> "\">"
damnFormat' C_A = "</a>"
damnFormat' (Abbr x) = "<abbr title=\"" <> x <> "\">"
damnFormat' C_Abbr = "</abbr>"
damnFormat' (Acro x) = "<acronym title=\"" <> x <> "\">"
damnFormat' C_Acro = "</acronym>"
damnFormat' (Avatar x _) = ":icon" <> x <> ":"
damnFormat' B = "<b>"
damnFormat' C_B = "</b>"
damnFormat' Bcode = "<bcode>"
damnFormat' C_Bcode = "</bcode>"
damnFormat' Br = "<br/>"
damnFormat' Code = "<code>"
damnFormat' C_Code = "</code>"
damnFormat' (Dev _ x) = ":dev" <> x <> ":"
damnFormat' (Embed x y z) =
    "<embed src=\"" <> x <> "\" height=\"" <> y <> "\" width=\"" <> z <> "\">"
damnFormat' C_Embed = "</embed>"
damnFormat' (Emote x _ _ _ _) = x
damnFormat' I = "<i>"
damnFormat' C_I = "</i>"
damnFormat' (Iframe x y z) =
    "<iframe src=\"" <> x <> "\" height=\"" <> y <> "\" width=\"" <> z <> "\">"
damnFormat' C_Iframe = "</iframe>"
damnFormat' (Img x y z) =
    "<img src=\"" <> x <> "\" height=\"" <> y <> "\" width=\"" <> z <> "\">"
damnFormat' Li = "<li>"
damnFormat' C_Li = "</li>"
damnFormat' (Link x _) = x
damnFormat' Ol = "<ol>"
damnFormat' C_Ol = "</ol>"
damnFormat' P = "<p>"
damnFormat' C_P = "</p>"
damnFormat' S = "<s>"
damnFormat' C_S = "</s>"
damnFormat' Sub = "<sub>"
damnFormat' C_Sub = "</sub>"
damnFormat' Sup = "<sup>"
damnFormat' C_Sup = "</sup>"
damnFormat' (Thumb x _ _ _ _ _) = ":thumb" <> x <> ":"
damnFormat' U = "<u>"
damnFormat' C_U = "</u>"
damnFormat' Ul = "<ul>"
damnFormat' C_Ul = "</ul>"
