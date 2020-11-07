{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.User where

import Import
import Yesod.Core.Json as YJ
import qualified Auth.JWT as JWT

postUserLoginR :: Handler YJ.Value
postUserLoginR = do
        jsonBody <- requireCheckJsonBody :: Handler Value
        case (checkUserNamePassword jsonBody) of
                Just v -> do
                        -- TESTING START
                        liftIO $ print.show $ v
                        access_token <- fakeGen $ (fst v) ++ "_in_jwt"
                        -- TESTING END
                        returnJson (object ["access_token" .= (access_token::Text)])
                _      -> sendResponseStatus unauthorized401 ()
        
checkUserNamePassword :: Value -> Maybe (String, String)
checkUserNamePassword (Object jb) = do
        (String username) <- lookup "username" jb 
        (String password) <- lookup "password" jb
        localCheck username password
        where localCheck "tester" "password" = Just ("tester","password")
              localCheck _        _          = Nothing
checkUserNamePassword _         = Nothing

 -- TESTING START
fakeGen :: JWT.UserId -> HandlerFor App Text
fakeGen userId = userIdToToken userId

fakeTest :: Text -> Handler ()
fakeTest token = do
        $(logInfo) token
        userId <- tokenToUserId token
        case userId of
                Just v -> liftIO $ print v
                _      -> sendResponseStatus unauthorized401 ()

getFakeR :: Handler YJ.Value
getFakeR = do
        lt <- JWT.lookupToken
        case lt of
                Just v -> fakeTest v
                Nothing -> sendResponseStatus unauthorized401 ()
        return "{}"
-- TESTING END