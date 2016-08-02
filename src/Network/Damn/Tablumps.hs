{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

module Network.Damn.Tablumps (
    module Network.Damn.Tablumps,
    Lump(..)
) where

import           Control.Applicative
import           Data.Attoparsec.ByteString       hiding (word8)
import qualified Data.Attoparsec.ByteString.Char8 as C
import           Data.ByteString                  (ByteString)
import           Data.Monoid
import           Data.Text                        (Text)
import           Data.Text.Encoding
import           Data.Text.Internal.Builder       (toLazyText)
import           Data.Text.Lazy                   (toStrict)
import           HTMLEntities.Decoder
import           Network.Damn.Tablumps.TH

-- type Lump = (ByteString, [ByteString])
-- data Lump = Lump deriving (Eq, Show)

tablumpP :: Parser [Either Text Lump]
tablumpP = many $ (Left . bytesToText <$> (C.takeWhile1 (/= '&')))
       <|> lump
       <|> fmap (Left . bytesToText) (C.string "&")

lump :: Parser (Either a Lump)
lump = (C.char '&' *>) $ choice
     [ $(ary 0 "b" 'B)
     , $(ary 0 "br" 'Br)
     , $(ary 0 "i" 'I)
     , $(ary 0 "u" 'U)

     , $(ary 0 "/abbr" 'CloseAbbr)
     , $(ary 0 "/b" 'CloseB)
     , $(ary 0 "/i" 'CloseI)
     , $(ary 0 "/u" 'CloseU)

     , $(ary 1 "abbr" 'Abbr)

     , $(ary 2 "dev" 'Dev)

     , $(ary 5 "emote" 'Emote)
     , $(ary 6 "thumb" 'Thumb)
     , link
     ]
    where
        -- ary n s = do
        --     string s
        --     C.char '\t'
        --     fmap (Right . (,) s) $ count n arg
        link = do
            _ <- string "link"
            _ <- C.char '\t'
            arg1 <- arg
            arg2 <- arg
            case arg2 of
                "&" -> pure $ Right $ Link arg1 Nothing
                _ -> do
                    _ <- string "&\t"
                    pure $ Right $ Link arg1 (Just arg2)
        arg = C.takeWhile (/= '\t') <* C.char '\t'

toLumps :: ByteString -> [Either Text Lump]
toLumps t = case parseOnly tablumpP t of
    Right y -> joinLefts y
    Left _  -> [Left $ bytesToText t]
    where
        joinLefts (Left a : Left b : xs) = joinLefts (Left (a <> b) : xs)
        joinLefts (x:xs)                 = x : joinLefts xs
        joinLefts []                     = []

bytesToText :: ByteString -> Text
bytesToText = htmlDecode . decodeLatin1

htmlDecode :: Text -> Text
htmlDecode = toStrict . toLazyText . htmlEncodedText
