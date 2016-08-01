{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as B8
import           Data.String
import           Network.Damn
import           Test.Hspec
import           Test.HUnit
import           Test.QuickCheck

main = hspec $
    describe "packets" $ do
        it "basic parsing" $
            "dAmnServer 0.3\n\0" @?= Just (Message "dAmnServer" (Just "0.3") [] Nothing)

        it "attrs" $
            "a\na=b\nc=d\n\0" @?= Just (Message "a" Nothing [("a","b"),("c","d")] Nothing)

        it "subpacket" $
            (subMessage =<< messageBody =<< "a\n\nb c\nd=e\n\0")
                @?= Just (SubMessage (Just "b") (Just "c") [("d","e")] Nothing)

instance IsString (Maybe Message) where
    fromString x = case parseMessage $ B8.pack x of
        Left _  -> Nothing
        Right x -> Just x
