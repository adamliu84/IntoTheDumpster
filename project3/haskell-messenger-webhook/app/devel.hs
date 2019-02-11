{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
import           Data.Text (Text, unpack)
import           Yesod
import           Network.HTTP.Types         (status200)
import           Network.Wai                (pathInfo, rawPathInfo,
                                             requestMethod, responseLBS)
import           Yesod
import qualified Data.Conduit.Text as CT (decode, utf8)
import qualified Data.Conduit.List as CL (consume)
import           Data.Conduit (runConduit, (.|))
import qualified Parrot
import           MessengerProc
import           Control.Monad (forM_)

hub_challenge = "hub_challenge_12345"
invalid_challenge = "invalid_challenge"

data App = App

mkYesod "App" [parseRoutes|
/hm_wh     WebhookR
/test      TestR
|]

instance Yesod App

handleWebhookR :: Handler Text
handleWebhookR = do
    req <- waiRequest
    hub_mode <- lookupGetParam "hub.mode"
    case hub_mode of
        Just v -> do
            hub_challenge <- lookupGetParam "hub.challenge"
            case hub_challenge of
                Just v -> return v
                _ -> do
                    return invalid_challenge
        _ -> do
            texts <- runConduit $ rawRequestBody .| CT.decode CT.utf8 .| CL.consume
            forM_ texts (\x -> do
                $(logDebug) x
                case (MessengerProc.genWebhook $ x) of
                    Just (MessagingReferrals v) ->  do $(logInfo) "Processing MessagingReferrals"
                                                       $(logInfo) v
                    Just (Messages v) ->            do $(logInfo) "Processing Messages"
                                                       liftIO $ Parrot.echoMessage v
                )
            return invalid_challenge

handleTestR :: Handler Text
handleTestR = do
    $(logInfo) "Testing\n"
    texts <- runConduit $ rawRequestBody .| CT.decode CT.utf8 .| CL.consume
    $(logInfo) (head texts)
    return "Testing"

main :: IO ()
main = warp 3000 App
