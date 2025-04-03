-- {-# LANGUAGE MultiParamTypeClasses #-}

class Convert a b where
  convert :: a -> b

instance Convert Int String where
  convert :: Int -> String
  convert = show

instance Convert String Int where
  convert :: String -> Int
  convert = read

main :: IO ()
main = do
    print $ convert @String @Int "111"
    print (convert "222" :: Int)
    print $ convert @Int @String 888
    print (convert (999 :: Int) :: String)

-- << OUTPUT
-- 111
-- 222
-- "888"
-- "999"
-- >>