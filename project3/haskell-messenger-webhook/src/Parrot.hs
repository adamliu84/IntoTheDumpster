{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Parrot (echoMessage) where

import qualified Data.ByteString.Lazy.Char8 as C8 (fromStrict)
import qualified Data.HashMap.Strict as DMS (lookup)
import qualified Data.Vector as DV (toList)
import qualified Data.Text.Encoding as TL (encodeUtf8)
import           Yesod
import           Data.Aeson
import           Data.Text (Text, unpack)
import           Network.Curl (curlPost)
import           Data.List (isPrefixOf)
import           WebhookUtil

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
    where message' = foldr replacement ("You have typed:\n" ++ message) replacementInput

echoMessage :: Text -> IO ()
echoMessage v =
    genSenderIdWithMessage (getSenderId v) (getMessageText v)
    where genSenderIdWithMessage (Just x) (Just y) = postMessage (unpack x) (unpack y)
          genSenderIdWithMessage _ _               = return ()

data Replacement = ReplacementCharacter (Char, String) | ReplacementString (String, String)

replacementInput :: [Replacement]
replacementInput =
     [ ReplacementCharacter ('&', "%26"),
       ReplacementCharacter ('\n', "%5Cr%5Cn"),
       ReplacementString ("\"", "%5C%22")
     ]

replacement :: Replacement -> String -> String
replacement (ReplacementCharacter x) m = replaceCharacter x m
replacement (ReplacementString x) m = replaceSubstring x m

replaceCharacter :: (Char, String) -> String -> String
replaceCharacter _ [] = []
replaceCharacter or'@(o,r') (m:ms)
    | m == o = r' ++ replaceCharacter or' ms
    | otherwise = m : replaceCharacter or' ms

replaceSubstring :: (String, String) -> String -> String
replaceSubstring _ [] = []
replaceSubstring or'@(o, r') m'@(m:ms)
    | o `isPrefixOf` m' = r' ++ replaceSubstring or' (drop (length o) m')
    | otherwise = m : replaceSubstring or' ms
