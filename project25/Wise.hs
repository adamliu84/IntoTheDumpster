{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

import           Data.ByteString.Internal   (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.List.Split            (splitOn)
import           Data.UUID                  (UUID)
import qualified Data.UUID.V4               as UUIDv4
import           Network.HTTP.Client        (Request,
                                             RequestBody (RequestBodyLBS),
                                             Response, applyBearerAuth, httpLbs,
                                             method, newManager, parseRequest,
                                             requestBody, requestHeaders,
                                             responseBody, responseStatus,
                                             urlEncodedBody)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)

bearer = "4_BEARER_KEY"
base_url_v1 = "https://api.sandbox.transferwise.tech/v1/"
base_url_v2 = "https://api.sandbox.transferwise.tech/v2/"
base_url_v3 = "https://api.sandbox.transferwise.tech/v3/"

dumpResponseCodeAndBody ::  Response BS.ByteString -> IO ()
dumpResponseCodeAndBody response = do
    print $ responseStatus response
    print $ responseBody response

sendRequest :: Request -> IO (Response BS.ByteString)
sendRequest initialRequest = do
    let request = applyBearerAuth bearer initialRequest
    manager <- newManager tlsManagerSettings
    httpLbs request manager

getProfileId :: IO String
getProfileId = do
    let endpoint = base_url_v2 ++ "profiles"
    initialRequest <- parseRequest endpoint
    response <- sendRequest (initialRequest  { method = "GET"})
    return $ praseFstProfileId $ responseBody response
    where
        praseFstProfileId :: BS.ByteString -> String
        praseFstProfileId str = head $ splitOn "," $ splitOn "profileId\":" (BS.unpack str)!!1

gen_recipient_account :: String -> String -> BS.ByteString
gen_recipient_account profileId lastName =  BS.pack $ "{\"profile\":"++profileId++",\"accountHolderName\":\"Jane "++lastName++"\",\"currency\":\"GBP\",\"type\":\"email\",\"details\":{\"email\":\""++lastName++"@transfer-world.com\"}}"

createRecipientAccount :: BS.ByteString -> IO String
createRecipientAccount sample_recipient_account = do
    let endpoint = base_url_v1 ++ "accounts"
    initialRequest <- parseRequest endpoint
    response <- sendRequest (initialRequest { method = "POST", requestBody = RequestBodyLBS sample_recipient_account, requestHeaders = [("Content-Type","application/json")]})
    return $ parseRecipientAccountId $ responseBody response
        where
        parseRecipientAccountId :: BS.ByteString -> String
        parseRecipientAccountId str = head $ splitOn "," $ splitOn "\"id\":" (BS.unpack str) !! 1

gen_auth_quote :: String -> BS.ByteString
gen_auth_quote recipientAccountId = BS.pack $ "{\"sourceCurrency\":\"GBP\",\"targetCurrency\":\"GBP\",\"sourceAmount\":168,\"targetAmount\":null,\"payOut\":\"BANK_TRANSFER\",\"preferredPayIn\":\"BALANCE\",\"targetAccount\":"++recipientAccountId++",\"paymentMetadata\":{\"transferNature\":\"MOVING_MONEY_BETWEEN_OWN_ACCOUNTS\"}}"

getAuthQuoteId :: String -> BS.ByteString -> IO String
getAuthQuoteId profileId sample_auth_quote = do
    let endpoint = base_url_v3 ++ "profiles/" ++ profileId ++ "/quotes"
    initialRequest <- parseRequest endpoint
    response <- sendRequest (initialRequest { method = "POST", requestBody = RequestBodyLBS sample_auth_quote, requestHeaders = [("Content-Type","application/json")]})
    return $ parseAuthQuoteId $ responseBody response
        where
        parseAuthQuoteId :: BS.ByteString -> String
        parseAuthQuoteId str = head $ splitOn "\"" $ splitOn "\"id\":\"" (BS.unpack str) !! 1

gen_refund_recipient_account :: String -> BS.ByteString
gen_refund_recipient_account profileId = BS.pack $ "{\"currency\":\"GBP\",\"country\":\"DE\",\"type\":\"sort_code\",\"profile\":"++profileId++",\"legalEntityType\":\"INSTITUTION\",\"name\":{\"fullName\":\"John Doe\"},\"accountHolderName\":\"John Doe\",\"details\":{\"sortCode\":\"040075\",\"accountNumber\":\"37778842\"}}"

createRefundRecipientAccount :: BS.ByteString -> IO String
createRefundRecipientAccount sample_refund_recipient_account = do
    let endpoint = base_url_v1 ++ "accounts"
    initialRequest <- parseRequest endpoint
    response <- sendRequest (initialRequest { method = "POST", requestBody = RequestBodyLBS sample_refund_recipient_account, requestHeaders = [("Content-Type","application/json")]})
    return $ parseRecipientAccountId $ responseBody response
        where
        parseRecipientAccountId :: BS.ByteString -> String
        parseRecipientAccountId str = head $ splitOn "," $ splitOn "\"id\":" (BS.unpack str) !! 1

gen_transfer :: String -> String -> String -> UUID -> BS.ByteString
gen_transfer refundAccountId recipientAccountId quoteId uuid = BS.pack $ "{\"sourceAccount\": " ++ refundAccountId ++ ",\"targetAccount\": "++ recipientAccountId ++",\"quoteUuid\":\""++ quoteId ++"\",\"customerTransactionId\":\""++ show uuid ++"\",\"details\": {  \"reference\":\"For Funding\"  }}"

createTransfer :: BS.ByteString -> IO String
createTransfer sample_transfer = do
    let endpoint = base_url_v1 ++ "transfers"
    initialRequest <- parseRequest endpoint
    response <- sendRequest (initialRequest { method = "POST", requestBody = RequestBodyLBS sample_transfer, requestHeaders = [("Content-Type","application/json")]})
    return $ parseTransferId $ responseBody response
     where
        parseTransferId :: BS.ByteString -> String
        parseTransferId str = head $ splitOn "," $ splitOn "\"id\":" (BS.unpack str) !! 1

gen_fund_balance :: BS.ByteString
gen_fund_balance = BS.pack "{\"type\":\"BALANCE\"}"

createFundTransfer :: String -> String -> BS.ByteString -> IO ()
createFundTransfer profileId transferId sample_fund_balance = do
    let endpoint = base_url_v3 ++ "profiles/"++ profileId ++"/transfers/"++ transferId ++"/payments"
    initialRequest <- parseRequest endpoint
    response <- sendRequest (initialRequest { method = "POST", requestBody = RequestBodyLBS sample_fund_balance, requestHeaders = [("Content-Type","application/json")]})
    dumpResponseCodeAndBody response
    return ()

main :: IO ()
main = do
    uuid <- UUIDv4.nextRandom
    let first_8 = take 8 $ show uuid
        rev_last_8 = take 8 . reverse $ show uuid
    profileId <- getProfileId
    recipientAccountId <- createRecipientAccount (gen_recipient_account profileId rev_last_8)
    quoteId <- getAuthQuoteId profileId (gen_auth_quote recipientAccountId)
    refundAccountId <- createRefundRecipientAccount (gen_refund_recipient_account profileId)
    transferId <- createTransfer (gen_transfer refundAccountId recipientAccountId quoteId uuid)
    createFundTransfer profileId transferId gen_fund_balance
    -- Simple Logging
    print $ "uuid " ++ show uuid
    print $ "profileId " ++ profileId
    print $ "recipientAccountId " ++ recipientAccountId
    print $ "quoteId " ++ quoteId
    print $ "refundAccountId " ++ refundAccountId
    print $ "transferId " ++ transferId
