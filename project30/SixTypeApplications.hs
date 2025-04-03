-- {-# LANGUAGE TypeApplications #-}

class FromString a where
  fromString :: String -> a

instance FromString Int where
  fromString :: String -> Int
  fromString = read

main :: IO ()
main = do
    let x = read @Double "999"
    print x
    print (fromString "123" :: Int)
    print (fromString @Int "123")

-- << OUTPUT
-- 999.0
-- 123
-- 123
--