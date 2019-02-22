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
import           WebhookUtil
import           Control.Monad (forM_)

hub_challenge = "hub_challenge_12345"
invalid_challenge = "invalid_challenge"

data App = App
instance Yesod App where
    -- This function controls which messages are logged
    shouldLogIO App src level = return $
        True -- good for development
        -- level == LevelWarn || level == LevelError -- good for production

mkYesod "App" [parseRoutes|
/hm_wh     WebhookR GET POST
/test      TestR
/duck_test DuckTestR POST
|]

getWebhookR :: Handler Text
getWebhookR = do
    req <- waiRequest
    verify_token <- lookupGetParam "hub.verify_token"
    case verify_token of
        Just "to_be_check" -> do
            hub_challenge <- lookupGetParam "hub.challenge"
            case hub_challenge of
                Just v -> return v
                _ -> do
                    return invalid_challenge
        _ -> return invalid_challenge

postWebhookR :: Handler ()
postWebhookR = do
    texts <- runConduit $ rawRequestBody .| CT.decode CT.utf8 .| CL.consume
    forM_ texts (\x -> do
        $(logDebug) x
        case (MessengerProc.genWebhook $ x) of
            Just (MessagingReferrals v) ->  do $(logInfo) "Processing MessagingReferrals"
                                               $(logInfo) v
            Just (Messages v) ->            do $(logInfo) "Processing Messages"
                                               liftIO $ Parrot.echoMessage v
            Nothing ->                      return ()
        )
    return ()

handleTestR :: Handler Text
handleTestR = do
    $(logInfo) "Testing\n"
    texts <- runConduit $ rawRequestBody .| CT.decode CT.utf8 .| CL.consume
    $(logInfo) (head texts)
    return "Testing"

postDuckTestR :: Handler ()
postDuckTestR = do
    quack <- lookupPostParam "quack"
    case quack of
        Just v  -> Parrot.ducktest v
        _       -> return ()

main :: IO ()
main = warp 3000 App
