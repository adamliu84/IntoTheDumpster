{-# LANGUAGE AllowAmbiguousTypes #-}

data Phantom

class ShowType a where
    showType :: String

instance ShowType Int where
    showType :: String
    showType = "This is an Int"

instance ShowType Bool where
    showType :: String
    showType = "This is a Bool"

instance ShowType Phantom where
    showType :: String
    showType = "This is a Phantom"

printType :: forall a. ShowType a => IO ()
printType = putStrLn (showType @a)

main :: IO ()
main = do
    printType @Int
    printType @Bool    
    putStrLn (showType @Phantom)

-- << OUTPUT
-- This is an Int
-- This is a Bool
-- This is a Phantom
-- >>