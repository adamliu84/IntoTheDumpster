{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Parrot (getSenderIdWithMessage, postMessage) where

import qualified Data.ByteString.Lazy.Char8 as C8 (fromStrict)
import qualified Data.HashMap.Strict as DMS (lookup)
import qualified Data.Vector as DV (toList)
import qualified Data.Text.Encoding as TL (encodeUtf8)
import           Yesod
import           Data.Aeson
import           Data.Text (Text)
import           Network.Curl (curlPost)

getObject :: Text -> Value -> Maybe Value
getObject k (Object v) = k `DMS.lookup` v
getObject _ _ = Nothing
getArray :: Int -> Value -> Maybe Value
getArray i (Array v) = Just ((DV.toList v)!!i)
getArray _ _ = Nothing
getString :: Value -> Maybe Text
getString (String v) = Just v
getString _ = Nothing

page_access_token :: String
page_access_token = "<PAGE_ACCESS_TOKEN>"
genEndPoint :: String
genEndPoint = concat ["https://graph.facebook.com/v2.6/",
                      "me/messages/",
                      "?",
                      "&access_token=",
                      page_access_token]

postMessage :: String -> String -> IO ()
postMessage psid message = do
    curlPost (genEndPoint) ["messaging_type=RESPONSE",
                            "recipient=%7B%0A%20%20%22id%22%3A%20%22"++psid++"%22%0A%7D",
                            "message=%7B%0A%20%20%22text%22%3A%20%22"++message'++"%22%0A%7D"
                           ]
    where message' = updateLineBreak $ "You have typed: " ++ message
          updateLineBreak [] = []
          updateLineBreak (x:xs)
            | x == '\n' = "%5Cr%5Cn" ++ updateLineBreak xs
            | otherwise = x : updateLineBreak xs

getSenderIdWithMessage :: Text -> Maybe (Text, Text)
getSenderIdWithMessage v =
    genSenderIdWithMessage (getSenderId v) (getMessageText v)
    where genSenderIdWithMessage (Just x) (Just y) = Just (x,y)
          genSenderIdWithMessage _ _               = Nothing

getMessaging0 :: Value -> Maybe Value
getMessaging0 v = getObject "entry" v
                  >>= getArray 0
                  >>= getObject "messaging"
                  >>= getArray 0

getSenderId :: Text -> Maybe Text
getSenderId v =
    (decode (C8.fromStrict $ TL.encodeUtf8 v) :: Maybe Value)
    >>= getMessaging0
    >>= getObject "sender"
    >>= getObject "id"
    >>= getString

getMessageText :: Text -> Maybe Text
getMessageText v =
    (decode (C8.fromStrict $ TL.encodeUtf8 v) :: Maybe Value)
    >>= getMessaging0
    >>= getObject "message"
    >>= getObject "text"
    >>= getString
