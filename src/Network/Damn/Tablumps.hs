{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

module Network.Damn.Tablumps (
    module Network.Damn.Tablumps,
    Lump(..)
) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString       hiding (word8)
import qualified Data.Attoparsec.ByteString.Char8 as C
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as B
import           Data.Monoid
import           Data.Text                        (Text, split)
import           Data.Text.Encoding
import           Data.Text.Internal.Builder       (toLazyText)
import           Data.Text.Lazy                   (toStrict)
import           Debug.Trace
import           HTMLEntities.Decoder
import           Network.Damn.Tablumps.TH

-- type Lump = (ByteString, [ByteString])
-- data Lump = Lump deriving (Eq, Show)

tablumpP :: Parser [Either Text Lump]
tablumpP = many $ (Left . bytesToText <$> (C.takeWhile1 (/= '&')))
       <|> lump
       <|> fmap (Left . bytesToText) (C.string "&")

lump = (C.char '&' *>) $
        $(ary 0 "br" 'Br)
    <|> $(ary 0 "/abbr" 'SlashAbbr)
    <|> $(ary 1 "abbr" 'Abbr)
    <|> $(ary 2 "dev" 'Dev)
    <|> $(ary 5 "emote" 'Emote)
    <|> $(ary 6 "thumb" 'Thumb)
    <|> link
    where
        -- ary n s = do
        --     string s
        --     C.char '\t'
        --     fmap (Right . (,) s) $ count n arg
        link = do
            string "link"
            C.char '\t'
            arg1 <- arg
            arg2 <- arg
            case arg2 of
                "&" -> pure $ Right $ Link arg1 Nothing
                _ -> do
                    string "&\t"
                    pure $ Right $ Link arg1 (Just arg2)
        arg = C.takeWhile (/= '\t') <* C.char '\t'

toLumps x = case parseOnly tablumpP x of
    Right y -> joinLefts y
    Left _  -> [Left $ bytesToText x]
    where
        joinLefts (Left a : Left b : xs) = joinLefts (Left (a <> b) : xs)
        joinLefts (x:xs)                 = x : joinLefts xs
        joinLefts []                     = []

bytesToText = htmlDecode . decodeLatin1
htmlDecode = toStrict . toLazyText . htmlEncodedText
