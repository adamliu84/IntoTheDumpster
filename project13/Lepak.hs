{-# LANGUAGE OverloadedStrings #-}

import System.Exit (die)
import Data.Char (isDigit)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as LB (ByteString, putStrLn, pack)

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
        ("Execute PUT", testHttpPut)
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
    let request =  initialRequest { method = "GET"}
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    dump response

testHttpPut :: IO ()
testHttpPut = do    
    initialRequest <- parseRequest (base_url ++ "/put?key99=value99")
    let request =  initialRequest { method = "PUT", requestBody = RequestBodyLBS sample_body_json}
    manager <- newManager tlsManagerSettings
    response <- httpLbs request manager
    dump response

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