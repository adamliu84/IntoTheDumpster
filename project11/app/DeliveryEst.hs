{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
-- https://developers.facebook.com/docs/marketing-api/reference/ad-account/delivery_estimate/

import Network.Wreq (defaults, param, getWith, responseBody)
import Control.Lens ((&), (.~), (^.))
import Data.Text (Text, pack)
import Data.List.Split (splitOn)
import Data.ByteString.Lazy.Char8 (unpack)
import Control.Exception
import System.IO (hFlush, stdout)

data Config = Config {ad_account_id :: String, access_token :: String} deriving (Show)

configFilename :: String
configFilename = "config.ini"

inputFilename :: String
inputFilename = "input.csv"

outputFilename :: String
outputFilename = "output.csv"

genEndPoint :: String -> String
genEndPoint ad_id = concat ["https://graph.facebook.com/v4.0/act_",ad_id,"/delivery_estimate"]

parseRow :: String -> (String, String)
parseRow row = (read y'::String, read (drop 1 z)::String)
    where (x,y) = break (\c -> c == ',') row
          (y',z) = break (\c -> c == ',') (drop 1 y)

genEstMau :: Config -> String -> String -> IO ()
genEstMau (Config {..}) i ts = do
  let opts = defaults & param "access_token" .~ [pack $ access_token]
             & param "targeting_spec" .~ [pack ts]
             & param "optimization_goal" .~ ["APP_INSTALLS"]
  r <- (getWith opts (genEndPoint (ad_account_id)))
  let response = r ^. responseBody
  appendFile outputFilename $ concat ["\"",i,"\"",",", getEstMau (unpack response),"\n"]

tryEstMau :: Config -> String -> String -> IO ()
tryEstMau config i ts =
  catch (genEstMau config i ts) (\(SomeException _) -> print $ concat ["error on row:",i])

getEstMau :: String -> String
getEstMau input = takeWhile (\c -> c /= ',')  ((splitOn "estimate_mau\":" input)!!1)

getConfig :: String -> Config
getConfig c = Config (drop 14 (c'!!0)) (drop 13 (c'!!1))
  where c' = lines c

main :: IO ()
main = do
  config <- (return.getConfig) =<< readFile configFilename
  contents <- readFile inputFilename
  let ts_rows = map parseRow (lines contents)
  mapM_ (\x -> tryEstMau config (fst x) (snd x) ) ts_rows
  putStrLn "Delivery Estimate Process Complete. Press enter to exit"
  hFlush stdout
  _ <-getLine
  return ()
