{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module MessengerProc (genWebhook) where

import qualified Data.ByteString.Lazy.Char8 as C8 (fromStrict)
import qualified Data.HashMap.Strict as DMS (lookup)
import qualified Data.Vector as DV (toList)
import qualified Data.Text.Encoding as TL (encodeUtf8)
import           Yesod
import           Data.Aeson
import           Data.Text (Text)
import           Control.Applicative ((<|>))
import           WebhookUtil

genWebhook :: Text -> Maybe WebhookEvent
genWebhook v = genMessagingReferrals v <|> genMessages v
