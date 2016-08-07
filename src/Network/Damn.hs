{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

-- | This module provides a datatype and convenience functions for parsing,
-- manipulating, and rendering deviantART Message Network messages.
module Network.Damn (
    -- *** Datatypes
    Message(..),
    SubMessage(..),
    MessageBody,
    -- *** Working with message bodies
    bodyBytes, Formatter, bodyWithFormat,
    toBody, toBodyText,
    -- *** Working with sub-messages
    subMessage,
    pattern SubM,
    -- *** Parsing
    parseMessage,
    messageP,
    -- *** Rendering
    render,
    -- *** Tablumps
    Lump(..)
) where

import           Control.Applicative
import qualified Control.Monad
import           Control.Monad.Fail
import           Data.Attoparsec.ByteString        hiding (word8)
import qualified Data.Attoparsec.ByteString        as A
import qualified Data.Attoparsec.ByteString.Char8  as C
import           Data.ByteString
import           Data.Char
import           Data.Ix
import           Data.Monoid
import           Data.String
import           Data.Text                         hiding (singleton)
import           Data.Word
import           Network.Damn.Format.Base          (Formatter)
import           Network.Damn.Format.Damn.Internal (textToBytes)
import           Network.Damn.Tablumps
import           Prelude                           hiding (fail)

-- | A top-level dAmn message.
--
-- General syntax for a message:
--
-- @
-- name arg
-- attr1=val1
-- attr2=val2
--
-- body
-- @
--
-- As reflected in the field types of 'Message', the 'arg' and 'body' are
-- both optional.
--
-- Attribute values are considered to be textual data and generally consist
-- of part reasons, privclass names, users' "taglines" and so on. The
-- message body can either be treated as text or as a 'SubMessage' (see
-- 'MessageBody').
--
-- Note that dAmn is a primarily browser-based platform; it deals with
-- only ISO-8859-1 output and input and inserts chat messages directly into
-- the DOM. As a consequence, correctly displaying characters past the ASCII
-- block requires the use of HTML entities.
--
-- All functions in this module transparently convert HTML entities
-- embedded in 'ByteString's to 'Text' (and back; see 'toBodyText'). Thus,
-- when 'Text' appears in fields of this record or of 'SubMessage', you can
-- assume that the HTML entity decoding step has already been handled.
data Message = Message
             { messageName     :: ByteString
             , messageArgument :: Maybe ByteString
             , messageAttrs    :: [(ByteString, Text)]
             , messageBody     :: Maybe MessageBody
             } deriving (Eq, Show)

-- | A second-level dAmn message. Note that this message can omit the
-- name/argument pair.
data SubMessage = SubMessage
                { subMessageName     :: Maybe ByteString
                , subMessageArgument :: Maybe ByteString
                , subMessageAttrs    :: [(ByteString, Text)]
                , subMessageBody     :: Maybe MessageBody
                } deriving (Eq, Show)

-- | The body of a message, which can be converted to various formats
-- ('bodyWithFormat') or parsed as a 'SubMessage' ('subMessage').
data MessageBody = MessageBody
                 { -- | View the original binary content of a 'MessageBody'.
                   --
                   -- To interpret this as textual data, use
                   -- 'bodyWithFormat'.
                   bodyBytes  :: ByteString
                   -- | Try to parse a 'MessageBody' as a 'SubMessage'.
                 , subMessage :: forall m. MonadFail m => m SubMessage
                 }

instance IsString MessageBody where
    fromString = toBody . fromString

-- bodyRaw (MessageBody b _) = show b
-- bodyText (MessageBody b _) = lumpsToText b
-- bodyTextInline (MessageBody b _) = lumpsToTextInline b

instance Show MessageBody where
    show (MessageBody b _) = show b

instance Eq MessageBody where
    MessageBody b _ == MessageBody b1 _ = b == b1

-- | 'subMessage' as a pattern.
--
-- @
-- case messageBody of
--     Sub (SubMessage name args attrs body)) -> ...
--     _ -> error "No parse"
-- @
--
-- Can be nested:
--
-- @
-- isJoinPacket :: Message -> Bool
-- isJoinPacket (Message "recv" room _
--     (Sub (SubMessage (Just "join") (Just uname) _
--         (Sub (SubMessage Nothing Nothing userAttrs _)))))
--     = True
-- isJoinPacket _ = False
-- @
pattern SubM :: SubMessage -> Maybe MessageBody
pattern SubM pkt <- ((>>= subMessage) -> Just pkt)

-- | Convert a 'MessageBody' to some stringlike representation using the
-- given 'Formatter'. (See 'Network.Damn.Format.Damn.damnFormat').
bodyWithFormat :: Monoid s => Formatter s -> MessageBody -> s
bodyWithFormat f = foldMap f . toLumps . bodyBytes

messageP :: Parser Message
messageP = do
    name <- C.takeWhile1 C.isAlpha_iso8859_15
    next <- C.peekChar'
    arg <- if next == ' '
               then C.char ' ' *> (Just <$> C.takeWhile1 (/= '\n'))
               else pure Nothing

    _ <- C.char '\n'
    attrs <- many attr

    body <- parseBody

    return $ Message name arg attrs body

parseBody :: Parser (Maybe MessageBody)
parseBody = do
    next <- C.anyChar
    case next of
        '\n' -> Just . toBody
            <$> Data.Attoparsec.ByteString.takeWhile (/= 0) <* A.word8 0
        '\0' -> pure Nothing
        _    -> Control.Monad.fail "Malformed packet"

subMessageP :: Parser SubMessage
subMessageP = do
    firstAttr <- optional attr
    case firstAttr of
        Just a -> do
            otherAttrs <- many attr
            body <- parseBody
            return $ SubMessage Nothing Nothing (a:otherAttrs) body
        Nothing -> do
            Message a b c d <- messageP
            return $ SubMessage (Just a) b c d

attr :: Parser (ByteString, Text)
attr = do
    k <- takeWhile1 nameChars
    _ <- C.char '='
    v <- C.takeWhile (/= '\n')
    _ <- C.char '\n'
    return (k, bytesToText v)

nameChars :: Word8 -> Bool
nameChars x = inRange (integralOrd 'a', integralOrd 'z') x
           || inRange (integralOrd 'A', integralOrd 'Z') x
           || inRange (integralOrd '0', integralOrd '9') x
    where integralOrd = fromIntegral . ord

-- | 'MessageBody' smart constructor.
toBody :: ByteString -> MessageBody
toBody x = MessageBody x
    (either Control.Monad.fail return $ parseOnly subMessageP (x <> "\0"))

-- | Like 'toBody', but convert codepoints outside the ASCII range to HTML
-- entities.
--
-- Note that this is NOT equivalent to @toBody . encodeUtf8@.
toBodyText :: Text -> MessageBody
toBodyText = toBody . textToBytes

-- | @'parseOnly' 'messageP'@
parseMessage :: ByteString -> Either String Message
parseMessage = parseOnly messageP

-- | Convert a 'Message' back into a 'ByteString' to send to dAmn. The null
-- byte is appended. In addition, all characters outside the ASCII block
-- are converted to HTML entities, thus
--
-- >>> render (Message "foo" (Just "bar") [("attr1", "☭")] Nothing)
-- "foo bar\nattr1=&#9773;\n\NUL"
render :: Message -> ByteString
render (Message name arg attrs body) = appendArg arg name
    <> "\n"
    <> renderAttrs attrs
    <> renderBody body
    <> "\0"
    where
        appendArg (Just b) = (<> (" " <> b))
        appendArg _        = id
        renderAttrs []         = ""
        renderAttrs ((a,b):bs) = a <> "=" <> textToBytes b <> "\n" <> renderAttrs bs
        renderBody (Just (MessageBody b _)) = "\n" <> b
        renderBody _                        = ""
