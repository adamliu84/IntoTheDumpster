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
import           Data.Conduit (($$), (=$))
import qualified Parrot

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
            texts <- rawRequestBody $$ CT.decode CT.utf8 =$ CL.consume
            case (Parrot.getSenderIdWithMessage $ head texts) of
                Just (x,y) -> do
                                liftIO $ Parrot.postMessage (unpack x) (unpack y)
                                $(logInfo) "Parrot back message"
                Nothing -> $(logInfo) "Parrot back nothing"
            return invalid_challenge

handleTestR :: Handler Text
handleTestR = do
    $(logInfo) "Testing\n"
    texts <- rawRequestBody $$ CT.decode CT.utf8 =$ CL.consume
    $(logInfo) (head texts)
    return "Testing"

main :: IO ()
main = warp 3000 App
