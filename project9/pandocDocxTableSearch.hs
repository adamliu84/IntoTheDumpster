-- with GHCi, version 8.4.3 &&
-- cabal-install version 2.2.0.0 compiled using version 2.2.0.1 of the Cabal library

import Text.Pandoc.Readers.Docx
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Data.ByteString.Lazy as LBS (readFile)
import Data.List (isInfixOf, isSuffixOf)
import System.Directory (getDirectoryContents)
import Data.Char (toLower)

------- CONFIG PARAM START -------

filePath :: String
filePath = "/YOUR_DOCX_ROOT_PATH/"

fileExtensions :: String
fileExtensions = "docx"

keywords :: String
keywords = "REPLACE_WITH_KEYWORDS"

targetCellAfterKeyWords :: Int
targetCellAfterKeyWords = 0

------- CONFIG PARAM END -------

getDirectoryDocxContent :: IO [String]
getDirectoryDocxContent = do
    getDirectoryContents filePath >>=
        \x -> return $
            filter (\y -> fileExtensions `isSuffixOf` [toLower loweredString | loweredString <- y]) x

defopts :: ReaderOptions
defopts = def{ readerExtensions = getDefaultExtensions fileExtensions }

readCurDocxContent :: String -> IO (String, Maybe String)
readCurDocxContent filename = do
    rawFile <- LBS.readFile (filePath++filename)
    (Pandoc _ dataBlock) <- runIOorExplode $ readDocx defopts rawFile
    return (filename, loopDataBlockTable dataBlock)

loopDataBlockTable :: [Block] -> Maybe String
loopDataBlockTable [] = Nothing
loopDataBlockTable (t@(Table _ _ _ _ rows):xs) =
    let searchResult = searchKeyValueCell $ genTableRowsContent
    in case searchResult of
        Just v -> Just v
        Nothing -> loopDataBlockTable xs
        where genTableRowsContent :: [String]
              genTableRowsContent =  map stringify (concat rows)
loopDataBlockTable (_:xs) = loopDataBlockTable xs

searchKeyValueCell :: [String] -> Maybe String
searchKeyValueCell [] = Nothing
searchKeyValueCell xxs@(x:xs)
    | keywords `isInfixOf` x = skipKeyValueCell targetCellAfterKeyWords xxs
    | otherwise = searchKeyValueCell xs
    where skipKeyValueCell :: Int -> [String] -> Maybe String
          skipKeyValueCell _ [] = Nothing
          skipKeyValueCell i (x:xs)
            | 0 == i = Just x
            | otherwise = skipKeyValueCell (pred i) xs

main :: IO ()
main = do
    docxFiles <- getDirectoryDocxContent
    mapM_ (\x -> print =<< readCurDocxContent x) docxFiles
