{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Directory
import Text.Pandoc.Readers.Docx
import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Data.ByteString.Lazy as LBS (readFile)
import System.Directory (getDirectoryContents)
import Data.Char (toLower)
import Data.Text (Text, unpack)
import System.Environment (getArgs)
import Text.Pandoc.Walk

type ExtractorOutput = (Text, Text)

defopts :: ReaderOptions
defopts = def{ readerExtensions = getDefaultExtensions "docx", readerTrackChanges = AllChanges}

main :: IO ()
main = do 
    args <- getArgs
    case args of
        [] -> print "Please append the filename at the back"
        [f] -> do
            rawFile <- LBS.readFile f
            (Pandoc _ dataBlock) <- runIOorExplode $ readDocx defopts rawFile    
            let a = query queryComment dataBlock
                writeIntoFile' = writeIntoFile (f ++ "_commentExtractor.csv")
            mapM_ (writeIntoFile') a

queryComment :: Block -> [ExtractorOutput]
queryComment (Para il)
    | length [ x | x@(Span (_,["comment-start"],_) _) <- il] > 0 = [trimAndExtractComment il]
    | otherwise = []
queryComment (Plain il)
    | length [ x | x@(Span (_,["comment-start"],_) _) <- il] > 0 = [trimAndExtractComment il]
    | otherwise = []
queryComment _ = []

trimAndExtractComment :: [Inline] -> ExtractorOutput
trimAndExtractComment = extractOutComment.trimOutComment

trimOutComment :: [Inline] -> [Inline]
trimOutComment inline = fst $ break tw $ dropWhile dw inline
    where
    dw :: Inline -> Bool
    dw (Span (_,["comment-start"],_) _) = False
    dw _ = True
    tw :: Inline -> Bool
    tw (Span (_,["comment-end"],_) _) = True
    tw _ = False

extractOutComment :: [Inline] -> ExtractorOutput
extractOutComment ((Span _ comment):zzz) =
    (stringify comment, stringify zzz)

writeIntoFile :: String -> (Text,Text) -> IO ()
writeIntoFile filename (c,s) = appendFile filename cs
    where c' = unpack c
          s' = unpack s
          cs = ("\""++c'++"\",\""++s'++"\"\n")