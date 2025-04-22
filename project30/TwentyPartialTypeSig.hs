{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

-- Inferring Return type
add' :: Int -> Int -> _
add' x y = x + y

-- Inferring Arg type
greet :: _ -> String
greet name = "Hello, " ++ name

-- Inferring Type Variables
firstElement :: [_] -> Maybe _
firstElement [] = Nothing
firstElement (x:_) = Just x

main :: IO ()
main = do
    print $ add' 5 12
    print $ greet "world!"
    print (firstElement [1, 2, 3])
    print (firstElement ["a", "b"])

-- << OUTPUT
-- 17
-- "Hello, world!"
-- Just 1
-- Just "a"
-- >>