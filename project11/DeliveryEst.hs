{-# LANGUAGE OverloadedStrings #-}

-- https://developers.facebook.com/docs/marketing-api/reference/ad-account/delivery_estimate/

import Network.Wreq (defaults, param, getWith, responseBody)
import Control.Lens ((&), (.~), (^.))
import Data.Text (Text, pack)
import Data.List.Split (splitOn)
import Data.ByteString.Lazy.Char8 (unpack)
import Control.Exception

ad_account :: String
ad_account = "act_<ID>"

access_token :: String
access_token = "<ACCESS_TOKEN>"

genEndPoint :: String
genEndPoint = concat ["https://graph.facebook.com/v4.0/",ad_account,"/delivery_estimate"]

parseRow :: String -> (String, String)
parseRow row = (read y'::String, read (drop 1 z)::String)
    where (x,y) = break (\c -> c == ',') row
          (y',z) = break (\c -> c == ',') (drop 1 y)

genEstMau :: String -> String -> IO ()
genEstMau i ts = do
  let opts = defaults & param "access_token" .~ [pack access_token]
             & param "targeting_spec" .~ [pack ts]
             & param "optimization_goal" .~ ["APP_INSTALLS"]
  r <- (getWith opts genEndPoint)
  let response = r ^. responseBody
  print $ concat [i,"->", getEstMau (unpack response)]

tryEstMau :: String -> String -> IO ()
tryEstMau i ts =
  catch (genEstMau i ts) (\(SomeException _) -> print $ concat ["error on row:",i])

getEstMau :: String -> String
getEstMau input = takeWhile (\c -> c /= ',')  ((splitOn "estimate_mau\":" input)!!1)

main :: IO ()
main = do
  contents <- readFile "temp.csv"
  let ts_rows = map parseRow (lines contents)
  mapM_ (\x -> tryEstMau (fst x) (snd x) ) ts_rows
