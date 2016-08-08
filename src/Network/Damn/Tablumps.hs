{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}

module Network.Damn.Tablumps (
    module Network.Damn.Tablumps,
    Lump(..)
) where

import           Control.Applicative
import           Control.Arrow                    (left)
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

tablumpP :: Parser [Either ByteString Lump]
tablumpP = many $ (Left <$> C.takeWhile1 (/= '&'))
       <|> lump
       <|> fmap Left (C.string "&")

lump :: Parser (Either a Lump)
lump = (C.char '&' *>) $ choice
     [ $(ary 2 "a" 'A)
     , $(ary 0 "/a" 'C_A)

     , $(ary 1 "abbr" 'Abbr)
     , $(ary 0 "/abbr" 'C_Abbr)

     , $(ary 1 "acro" 'Acro)
     , $(ary 0 "/acro" 'C_Acro)

     , $(ary 2 "avatar" 'Avatar)

     , $(ary 0 "b" 'B)
     , $(ary 0 "/b" 'C_B)

     , $(ary 0 "bcode" 'Bcode)
     , $(ary 0 "/bcode" 'C_Bcode)

     , $(ary 0 "br" 'Br)

     , $(ary 0 "code" 'Code)
     , $(ary 0 "/code" 'C_Code)

     , $(ary 2 "dev" 'Dev)

     , $(ary 3 "embed" 'Embed)
     , $(ary 0 "/embed" 'C_Embed)

     , $(ary 5 "emote" 'Emote)

     , $(ary 0 "i" 'I)
     , $(ary 0 "/i" 'C_I)

     , $(ary 3 "iframe" 'Iframe)
     , $(ary 0 "/iframe" 'C_Iframe)

     , $(ary 3 "img" 'Img)

     , $(ary 0 "li" 'Li)
     , $(ary 0 "/li" 'C_Li)

     , link

     , $(ary 0 "ol" 'Ol)
     , $(ary 0 "/ol" 'C_Ol)

     , $(ary 0 "p" 'P)
     , $(ary 0 "/p" 'C_P)

     , $(ary 0 "s" 'S)
     , $(ary 0 "/s" 'C_S)

     , $(ary 0 "sub" 'Sub)
     , $(ary 0 "/sub" 'C_Sub)

     , $(ary 0 "sup" 'Sup)
     , $(ary 0 "/sup" 'C_Sup)

     , $(ary 6 "thumb" 'Thumb)

     , $(ary 0 "u" 'U)
     , $(ary 0 "/u" 'C_U)

     , $(ary 0 "ul" 'Ul)
     , $(ary 0 "/ul" 'C_Ul)
     ]
    where
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
    Right y -> map (left (htmlDecode . bytesToText)) $ joinLefts y
    Left _  -> [Left $ bytesToText t]
    where
        joinLefts (Left a : Left b : xs) = joinLefts (Left (a <> b) : xs)
        joinLefts (x:xs)                 = x : joinLefts xs
        joinLefts []                     = []

bytesToText :: ByteString -> Text
bytesToText = decodeLatin1

htmlDecode :: Text -> Text
htmlDecode = toStrict . toLazyText . htmlEncodedText
