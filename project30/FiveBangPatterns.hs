{-# LANGUAGE BangPatterns #-}
import System.TimeIt

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs -- Lazy

strictSumList :: [Int] -> Int
strictSumList [] = 0
strictSumList (x:xs) = let !restSum = strictSumList xs in x + restSum -- Strict in the recursive call's result

sumLazy :: [Int] -> Int
sumLazy = foldl (+) 0  -- Uses a lazy left fold

sumStrict :: [Int] -> Int
sumStrict = go 0
  where
    go !acc []     = acc  -- `!acc` forces evaluation of accumulator
    go !acc (x:xs) = go (acc + x) xs  -- Strictly accumulates the sum


main :: IO ()
main = do
    timeIt $ do
        let i0 = [1..999999]
        print $ sumList i0
    
    timeIt $ do
        let i1 = [1..999999]
        print $ strictSumList i1

    timeIt $ do
        let i2 = [1..999999]
        print $ sumLazy i2

    timeIt $ do
        let i3 = [1..999999]
        print $ strictSumList i3

    timeIt $ do
        let i99 = [1..999999]
        print $ sum i99

-- << OUTPUT (Also test out timeIt)
-- 499999500000
-- CPU time:   0.29s
-- 499999500000
-- CPU time:   0.36s
-- 499999500000
-- CPU time:   0.15s
-- 499999500000
-- CPU time:   0.32s
-- 499999500000
-- CPU time:   0.03s
-- >>