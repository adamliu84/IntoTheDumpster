{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Parrot (echoMessage, ducktest) where

import           Yesod
import           Data.Text (Text, unpack)
import           Network.Curl (curlPost)
import           Data.List (isPrefixOf)
import           WebhookUtil
import           Duckling.Core
import           Duckling.Dimensions
import           Duckling.Testing.Types

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

ducktest :: (MonadLogger m) => Text -> m ()
ducktest message = do
    let
        parseResult = Duckling.Core.parse message testContext testOptions (Duckling.Dimensions.allDimensions EN)
    case (length parseResult) of
        0 -> do return ()
        _ -> do $(logInfo) (toJText (head parseResult))
