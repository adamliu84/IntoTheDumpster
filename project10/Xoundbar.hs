-- https://www.facebook.com/GovTechSG/photos/a.411947277510/10155774602792511/?type=3&theater

import Data.Char (ord, chr, isAlpha)

input = "owdbmkp eqapma gwc i pixxg ivl xzwaxmzwca tcviz vme gmiz. pcib ip!"
magicNum = 8

shiftChar :: String -> String
shiftChar [] = []
shiftChar (c:cs)
    | isAlpha c = c' : shiftChar cs
    | otherwise = c : shiftChar cs
    where c' :: Char
          c' = chr $ if v < 97 then 122 - (96 - v) else v
          v  :: Int
          v = ord c - magicNum

main :: IO ()
main = do
    print $ shiftChar input
