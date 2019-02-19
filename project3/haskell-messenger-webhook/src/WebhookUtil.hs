{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- module WebhookUtil (WebhookEvent(..), genMessagingReferrals, genMessages, getSenderId, getMessageText) where
module WebhookUtil where

import qualified Data.ByteString.Lazy.Char8 as C8 (fromStrict)
import qualified Data.HashMap.Strict as DMS (lookup)
import qualified Data.Vector as DV (toList)
import qualified Data.Text.Encoding as TL (encodeUtf8)
import           Yesod
import           Data.Aeson
import           Data.Text (Text)

getObject :: Text -> Value -> Maybe Value
getObject k (Object v) = k `DMS.lookup` v
getObject _ _ = Nothing
getArray :: Int -> Value -> Maybe Value
getArray i (Array v) = Just ((DV.toList v)!!i)
getArray _ _ = Nothing
getString :: Value -> Maybe Text
getString (String v) = Just v
getString _ = Nothing

data WebhookEvent = Messages Text | MessagingReferrals Text deriving (Show)

genMessagingReferrals :: Text -> Maybe WebhookEvent
genMessagingReferrals v = getMessaging0 v
                            >>= getObject "postback"
                            >>= getObject "referral"
                            >>= getObject "ad_id"
                            >>= getString
                            >>= (\_ -> return (MessagingReferrals v))

genMessages :: Text -> Maybe WebhookEvent
genMessages v = getMessaging0 v
                >>= getObject "message"
                >>= getObject "text"
                >>= (\_ -> return (Messages v))

getMessaging0 :: Text -> Maybe Value
getMessaging0 v = (decode (C8.fromStrict $ TL.encodeUtf8 v) :: Maybe Value)
                  >>= getObject "entry"
                  >>= getArray 0
                  >>= getObject "messaging"
                  >>= getArray 0

getSenderId :: Text -> Maybe Text
getSenderId v = getMessaging0 v
                >>= getObject "sender"
                >>= getObject "id"
                >>= getString

getMessageText :: Text -> Maybe Text
getMessageText v = getMessaging0 v
                   >>= getObject "message"
                   >>= getObject "text"
                   >>= getString
