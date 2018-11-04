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
    let target = getTargetValue dataBlock
    return (filename, target)

getTargetValue :: [Block] -> Maybe String
getTargetValue dataBlock =
    searchKeyValueCell $
    concat $ map (genTableRowsContent) $
    filter isTable dataBlock

isTable :: Block -> Bool
isTable (Table _ _ _ _ _) = True
isTable _ = False

genTableRowsContent :: Block -> [String]
genTableRowsContent rs =  map stringify (concat.getTableRows $ rs)
    where getTableRows :: Block -> [[TableCell]]
          getTableRows (Table _ _ _ _ rows) = rows

searchKeyValueCell :: [String] -> Maybe String
searchKeyValueCell [] = Nothing
searchKeyValueCell (x:xs)
    | keywords `isInfixOf` x = searchTargetValueCell targetCellAfterKeyWords xs
    | otherwise = searchKeyValueCell xs
    where searchTargetValueCell :: Int -> [String] -> Maybe String
          searchTargetValueCell _ [] = Nothing
          searchTargetValueCell i (x:xs)
            | 0 == i = Just x
            | otherwise = searchTargetValueCell (pred i) xs

main :: IO ()
main = do
    docxFiles <- getDirectoryDocxContent
    mapM_ (\x -> print =<< readCurDocxContent x) docxFiles
