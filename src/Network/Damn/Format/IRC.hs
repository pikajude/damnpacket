{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Format a dAmn message body as an inline text approximation, including
-- IRC styles.
module Network.Damn.Format.IRC (
    ircFormat, Lines, unLines
) where

import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as B
import           Data.String
import           Data.Text.Encoding
import           Network.Damn.Format.Base
import           Network.Damn.Format.Damn.Internal
#if __GLASGOW_HASKELL__ <= 708
import           Data.Monoid
#endif

newtype Lines = Lines ByteString deriving (Show, Monoid, IsString)

unLines :: Lines -> [ByteString]
unLines (Lines bs) = B.split 10 bs

-- | This formatter functions as 'Network.Damn.Format.Damn.damnFormat',
-- except that bold, italics, and underlines will be converted to mIRC
-- colors.
--
-- Additionally, @Text@ will be encoded as UTF-8.
--
-- Note that this formatter will generate 'Lines', as newlines (to which
-- @&br\\t@ translates) cannot appear in IRC messages.
ircFormat :: Formatter Lines
ircFormat = either (Lines . encodeUtf8) (Lines . ircFormat')

ircFormat' :: Lump -> ByteString
ircFormat' B   = "\x02"
ircFormat' C_B = "\x02"
ircFormat' Br  = "\n"
ircFormat' I   = "\x1D"
ircFormat' C_I = "\x1D"
ircFormat' U   = "\x1F"
ircFormat' C_U = "\x1F"
ircFormat' x   = damnFormat' x
