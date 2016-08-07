{-# LANGUAGE OverloadedStrings #-}

module Network.Damn.Format.Damn (
    damnFormat,
    damnFormatBytes
) where

import Data.ByteString                   (ByteString)
import Data.Text.Encoding
import Network.Damn.Format.Base
import Network.Damn.Format.Damn.Internal

-- | @damnFormat@ is, essentially, a formatter that transforms a raw
-- dAmn message into the text that the sender typed into the dAmn web
-- client.
damnFormat :: Formatter Text
damnFormat = either id (decodeUtf8 . damnFormat')

-- | @encodeUtf8 . damnFormat@
--
-- This function is defined separately to avoid an encoding round-trip.
damnFormatBytes :: Formatter ByteString
damnFormatBytes = either encodeUtf8 damnFormat'
