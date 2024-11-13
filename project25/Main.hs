{-# LANGUAGE OverloadedStrings #-}

import Data.Digest.Pure.SHA (showDigest, hmacSha256)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.HTTP.Client (Response, parseRequest, newManager, httpLbs, responseStatus, method, requestHeaders, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.ByteString.Internal (ByteString)

{-|
CONST
--}
apiKey="APIKEY====="
secret="SECRET====="
request_apikey_header = [("X-MBX-APIKEY",apiKey)]
sample_order_param = concat ["symbol=XRPBTC&side=BUY&type=LIMIT&timeInForce=GTC&quantity=5&price=0.00002000"]
root_domain = "testnet.binance.vision"
base_url = concat ["https://", root_domain]

{-|
FUNCTION
--}
dumpResponseCodeAndBody ::  Response BS.ByteString -> IO ()
dumpResponseCodeAndBody response = do
    print $ responseStatus response
    print $ responseBody response

getResponse :: String -> Data.ByteString.Internal.ByteString -> IO (Response BS.ByteString)
getResponse url method = do
    initialRequest <- parseRequest url
    let request =  initialRequest { method = method, requestHeaders = request_apikey_header}
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    return response

executeOrder :: IO ()
executeOrder = do
    t <- getCurTime
    let paramsWithTs = generateOrderParam t
        signature = showDigest $ hmacSha256 secret (BS.pack $ paramsWithTs)
        order_api_call_url = concat [base_url, "/api/v3/order?", paramsWithTs,"&signature=", signature] 
    response <- getResponse order_api_call_url "POST"    
    dumpResponseCodeAndBody response 
    where
    generateOrderParam :: Int -> String
    generateOrderParam t = concat [sample_order_param, "&timestamp=", show t]
    getCurTime :: IO Int
    getCurTime = do
        (floor `fmap` getPOSIXTime) >>= (\x -> return $ x * 1000)

{-|
MAIN
--}
main :: IO ()
main = executeOrder