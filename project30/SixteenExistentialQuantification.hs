-- {-# LANGUAGE ExistentialQuantification #-}

class Printable a where
    printMe :: a -> String

data Showable = forall a. (Printable a, Show a) => Showable a
-- GADT Alternative ↓
-- data Showable where
--     Showable :: (Printable a, Show a) => a -> Showable

instance Printable Int where
    printMe :: Int -> String
    printMe x = "Integer: " ++ show x

instance Printable String where
    printMe :: String -> String
    printMe x = "String: " ++ x

instance Printable Double where
    printMe :: Double -> String
    printMe x = "Double: " ++ show x

printShowable :: Showable -> String
printShowable (Showable x) = printMe x

main :: IO ()
main = do
    let thingsToPrint = [Showable "Hello World", Showable @Int 3, Showable @Double 6]
    mapM_ (putStrLn. printShowable) thingsToPrint
    mapM_ (\(Showable x) -> print x) thingsToPrint

-- <<OUTPUT
-- String: Hello World
-- Integer: 3
-- Double: 6.0
-- "Hello World"
-- 3
-- 6.0
-- >>
