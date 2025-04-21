{-# LANGUAGE TemplateHaskell #-}
module Main where

import EighteenTemplateHTH

five_factorial = $(factorialQ 5)

$(fibQ 20)
{-
WARNING: Generate 20 helloFib func
helloFib0   helloFib11  helloFib14  helloFib17  helloFib2   helloFib4   helloFib7
helloFib1   helloFib12  helloFib15  helloFib18  helloFib20  helloFib5   helloFib8
helloFib10  helloFib13  helloFib16  helloFib19  helloFib3   helloFib6   helloFib9
-}
fibonacci_10 = helloFib10 -- See fibNameTemplate = "helloFib"
$(genRenamedFunc "double")
$(genRenamedFunc "twice")
$(genRenamedFunc "timesTwo")

-- :t func1
-- func1 :: (Int, Int, Int) -> Int
func1 :: $(tupleTypeQ 3) 
func1 $(tuplePatQ 3) = (x1 * 10) + x2 + x3

-- :t func2
-- func2 :: ((Int, Int, Int) -> Int) -> Int
func2 :: $(tupleTypeQ 3) -> Int 
func2 f = f (9,80,700)

-- :t func2
-- func2 :: Int -> Int -> Int -> Int -> Int
func3 :: $(functionTypeQ 4)
func3 a b c d = a + b + c + d

{-
myfilter  myhello   mymap
-}
hello :: String -> String
hello name = "Hello " ++ name
$(prefixFunctions "my" ['map, 'filter, 'hello])

main :: IO ()
main = do
    putStrLn $ "5! = " ++ show five_factorial
    putStrLn $ "Fibonacci 10 = " ++ show fibonacci_10
    print $ double 5
    print $ twice 7
    print $ timesTwo 9

    print $ func1 (1,200,3000)
    print $ func2 (\(x,y,z) -> x + y +z)

    print $ func3 1 2 3 4

    print $ mymap succ [1..10]
    print $ myfilter even [1..10]
    print $ myhello "world"


-- << OUTPUT
-- 5! = 120
-- Fibonacci 10 = 55
-- 6
-- 8
-- 10
-- 3210
-- 789
-- 10
-- [2,3,4,5,6,7,8,9,10,11]
-- [2,4,6,8,10]
-- "Hello world"
-- >>