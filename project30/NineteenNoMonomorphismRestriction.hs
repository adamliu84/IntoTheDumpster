{-# LANGUAGE NoMonomorphismRestriction #-}
myValue = read "123" -- :t myValue ~> myValue :: Read a => a

myFuncInt :: Integer -> Integer
myFuncInt x = x + myValue

myFuncDouble :: Double -> Double
myFuncDouble x = x + myValue

main :: IO ()
main = do
    print (myFuncInt 5)
    print (myFuncDouble 3.14)

-- << OUTPUT
-- 128
-- 126.14
-- >>