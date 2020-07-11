{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import System.Exit (die)
import Data.Char (isDigit)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as LB (ByteString, putStrLn, pack, writeFile)
import qualified Data.ByteString.Char8 as B
import Control.Exception (SomeException, catch)

type CommandList = (Int, (String, IO ()))

base_url :: String
base_url = "https://httpbin.org/"

sample_body_json :: LB.ByteString
sample_body_json = LB.pack "{\"name\":\"Joe\",\"age\":12}"

{-|
HELPER FUNCTIONS
--}
printCmdList :: [CommandList] -> IO ()
printCmdList cmdList = do
    putStrLn "List of commands:"
    mapM_ (\(x,(y,_)) -> putStrLn $ (show x) ++ ": " ++ y) cmdList
    putStrLn "q: To quit gracefully"
    putStrLn "-----------"

dump :: Response LB.ByteString -> IO ()
dump response = do
    putStrLn "Status:"
    putStrLn.show.responseStatus $ response
    putStrLn "Headers:"    
    mapM_ print $ responseHeaders response
    putStrLn "Body:"
    LB.putStrLn $ responseBody response        

{-|
HTTP METHODS Testing different http verbs
Not creating helper function to generate the reqeust, showing full code
--}
testHttpGet :: IO ()
testHttpGet = do
    initialRequest <- parseRequest (base_url ++ "/get?key1=value1&key2=value2")
    let request = initialRequest { method = "GET"}
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    dump response

testHttpPut :: IO ()
testHttpPut = do    
    initialRequest <- parseRequest (base_url ++ "/put?key99=value99")
    let request = initialRequest { method = "PUT", requestBody = RequestBodyLBS sample_body_json}
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    dump response

{-|
AUTH METHODS
--}
testAuthBasicAuth :: IO ()
testAuthBasicAuth = do
    let user = "user"
        password = "password"
    initialRequest <- parseRequest (base_url ++ "/basic-auth/" ++ user ++ "/" ++ password)
    let request = applyBasicAuth (B.pack user) (B.pack password) $ initialRequest { method = "GET"}
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    dump response

testAuthBearer :: IO ()
testAuthBearer = do
    initialRequest <- parseRequest (base_url ++ "/bearer")
    let request = initialRequest { method = "GET", requestHeaders = [("Authorization","Bearer Authorization")]}
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    dump response

{-|
STATUS CODES Generates responses with given status code
--}
testStatusCodePost :: IO ()
testStatusCodePost = do    
    initialRequest <- parseRequest (base_url ++ "/status/100%2C200%2C300%2C400%2C500")
    let request = initialRequest { method = "POST"}
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    dump response

{-|
REQUEST INSPECTION Inspect the request data
--}
testRequestInspectionAll :: IO ()
testRequestInspectionAll = do
    let allMethods = [("/headers", []), ("/ip", []), ("/user-agent", [("user-agent","What do the fox say")])]
    flip mapM_ allMethods
        (\(u,h) -> do
            initialRequest <- parseRequest (base_url ++ u)
            let request = initialRequest { method = "GET", requestHeaders = h}
            manager <- newManager tlsManagerSettings
            response <- httpLbs request manager
            dump response
        )

{-|
RESPONSE INSPECTION Inspect the RESPONSE data
--}
testResponseInspectionCache :: IO ()
testResponseInspectionCache = do
    initialRequest <- parseRequest (base_url ++ "/cache")
    -- To trigger 304
    let request = initialRequest { method = "GET", requestHeaders = [("If-Modified-Since","Wed, 21 Oct 2015 07:28:00 GMT ")]}
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    dump response

testResponseInspectionPost :: IO ()
testResponseInspectionPost = do
    initialRequest <- parseRequest (base_url ++ "/response-headers")
    let request = setQueryString [("freeform", Just ("qwe 123 !@#"))] $
                    initialRequest { method = "POST"}
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    dump response

{-|
RESPONSE FORMATS Returns responses in different dta formats
--}
testResponseFormatUtf8 :: IO ()
testResponseFormatUtf8 = do
    initialRequest <- parseRequest (base_url ++ "/encoding/utf8")
    let request = initialRequest { method = "GET"}
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    dump response

testResponseFormatRobot :: IO ()
testResponseFormatRobot = do
    initialRequest <- parseRequest (base_url ++ "/robots.txt")
    let request = initialRequest { method = "GET", requestHeaders = [("accept","text/plain")]}
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    dump response

testResponseFormatHtmlXml :: IO ()
testResponseFormatHtmlXml = do
    let allMethods = [("/html", []), ("/xml", [("accept","application/xml")])]
    flip mapM_ allMethods
        (\(u,h) -> do
            initialRequest <- parseRequest (base_url ++ u)
            let request = initialRequest { method = "GET", requestHeaders = h}
            manager <- newManager tlsManagerSettings
            response <- httpLbs request manager
            dump response
        )

{-|
DYNAMIC DATA Generates random and dynamic data
--}
testDynamicDataDelay = do
    -- Request set to be 1 sec, response to be 3 sec to trigger error
    initialRequest <- parseRequest (base_url ++ "/delay/3") -- Response to be 3 sec delay
    let resTimeout = responseTimeoutMicro 1000000 -- Set timeout as 1 sec only
        request = initialRequest { method = "DELETE", responseTimeout = resTimeout}
    manager <- newManager tlsManagerSettings
    catch
        (httpLbs request manager >>= dump)
        (\(ex :: SomeException) -> putStrLn $ "Caught exception: " ++ show ex)

{-|
IMAGES Returns image formats
--}
testImageWriteJpeg :: IO ()
testImageWriteJpeg = do
    initialRequest <- parseRequest (base_url ++ "/image/jpeg")
    let request = initialRequest { method = "GET", requestHeaders = [("accept","image/jpg")]}
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    LB.writeFile "temp.jpg" $ responseBody response

main :: IO ()
main = do
    let loop = do
        let cmdList = getCmdList
        printCmdList cmdList
        cmd <- getLine
        case cmd of
            "q" -> die "Exiting....."            
            _   -> if (all isDigit cmd) then
                    case (lookup (read cmd :: Int) cmdList) of
                        Just v  -> snd v
                        Nothing -> putStrLn "Not within the function call"
                   else
                    error "Invalid command, error exit"
        loop
    loop
    where getCmdList :: [CommandList]
          getCmdList = zip [1..]
            [
                ("Execute GET", testHttpGet),
                ("Execute PUT", testHttpPut),
                ("HTTP Basic Auth", testAuthBasicAuth),
                ("Bearer Auth", testAuthBearer),
                ("Status code POST", testStatusCodePost),
                ("Request inspection", testRequestInspectionAll),
                ("Response inspection cache", testResponseInspectionCache),
                ("Response inspection post", testResponseInspectionPost),
                ("Response formats utf8", testResponseFormatUtf8),
                ("Response formats robot.txt rules", testResponseFormatRobot),
                ("Response formats html & xml return", testResponseFormatHtmlXml),
                ("Images read and create jpg image", testImageWriteJpeg),
                ("Response delay test", testDynamicDataDelay)
            ]