import System.Directory (doesFileExist)
import Control.Monad (when)
import System.Environment (getArgs)

access_token :: String
access_token = "default9876543210accesstoken0123456789"

data ConfigFile = ConfigFile{ path::String,
                              filename::String,
                              prefixPattern::String,
                              postfixPattern::String}

genFullPath :: ConfigFile -> String
genFullPath (ConfigFile p f _ _) = p++f

config_files :: [ConfigFile]
config_files = [(ConfigFile "./Levela/" "t.php" "'access_token' => " ""),
                (ConfigFile "./Level1/Level2/" "t.php" "'access_token' => " ",")]

updateToken :: ConfigFile -> String -> String -> String
updateToken config cur newtoken = case (take (length prefix) cur == prefix) of
                    True -> concat [prefix, "'", newtoken, "'", postfix]
                    False -> cur
                    where prefix = prefixPattern config
                          postfix = postfixPattern config

checkFile :: ConfigFile -> String -> IO ()
checkFile config newtoken = do
    exist <- doesFileExist $ genFullPath config
    when exist $ do
            ls <- fmap lines (readFile $ genFullPath config)
            let result = foldr (\x acc -> (updateToken config x newtoken):acc) [] ls
            (length result) `seq` writeFile (genFullPath config) (unlines result)

main :: IO ()
main = do
    newtoken <- (return.getToken) =<< getArgs
    print $ "Replacing all config files with new token:" ++ newtoken
    mapM_ (flip checkFile newtoken) (config_files)
    print "fin"
    where getToken = \x -> case (length x) of
                        0 -> access_token
                        _ -> (x!!0)
