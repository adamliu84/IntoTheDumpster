import System.Directory (doesFileExist)
import Control.Monad (when)
import System.Environment (getArgs)

access_token :: String
access_token = "default9876543210accesstoken0123456789"

data ConfigFile = ConfigFile{ path::String,
                              filename::String,
                              prefixPattern::String}

genFullPath :: ConfigFile -> String
genFullPath (ConfigFile p f _) = p++f

config_files :: [ConfigFile]
config_files = [(ConfigFile "./Levela/" "t.php" "'access_token' => "),
                (ConfigFile "./Level1/Level2/" "t.php" "'access_token' => ")]

updateToken :: String -> String -> String -> String
updateToken prefix cur newtoken = case (take (length prefix) cur == prefix) of
                    True -> concat [prefix, "'", newtoken, "'"]
                    False -> cur

checkFile :: ConfigFile -> String -> IO ()
checkFile cur newtoken = do
    exist <- doesFileExist $ genFullPath cur
    when exist $ do
            ls <- fmap lines (readFile $ genFullPath cur)
            let result = foldr (\x acc -> (updateToken prefix x newtoken):acc) [] ls
            (length result) `seq` writeFile (genFullPath cur) (unlines result)
            where prefix = prefixPattern cur

main :: IO ()
main = do
    newtoken <- (return.getToken) =<< getArgs
    print $ "Replacing all config files with new token:" ++ newtoken
    mapM_ (flip checkFile newtoken) (config_files)
    print "fin"
    where getToken = \x -> case (length x) of
                        0 -> access_token
                        _ -> (x!!0)
