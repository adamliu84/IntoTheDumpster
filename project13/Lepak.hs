{-# LANGUAGE OverloadedStrings #-}

import System.Exit (die)
import Data.Char (isDigit)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as LB (ByteString, putStrLn, pack)
import qualified Data.ByteString.Char8 as B

type CommandList = (Int, (String, IO ()))

base_url :: String
base_url = "https://httpbin.org/"

sample_body_json :: LB.ByteString
sample_body_json = LB.pack "{\"name\":\"Joe\",\"age\":12}"

{-|
HELPER FUNCTIONS
--}
getCmdList :: [CommandList]
getCmdList = zip [1..] 
    [
        ("Execute GET", testHttpGet),
        ("Execute PUT", testHttpPut),
        ("HTTP Basic Auth", testAuthBasicAuth),
        ("Bearer Auth", testAuthBearer),
        ("Status code POST", testStatusCodePost),
        ("Request inspection", testRequestInspectionAll)
    ] 

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
testRequestInspectionAll = do
    let allMethods = [("/headers", []), ("/ip", []), ("/user-agent", [("user-agent","What do the fox say")])]
    flip mapM_ (allMethods)
        (\(u,h) -> do
            initialRequest <- parseRequest (base_url ++ u)
            let request = initialRequest { method = "GET", requestHeaders = h}
            manager <- newManager tlsManagerSettings
            response <- httpLbs request manager
            dump response
        )

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