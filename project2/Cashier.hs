import Data.List

priceList = [1.25, 1.35, 2.35]
purchasedNumber = 5
purchasedAmount = 8.65

bruteList :: [(Double,Double,Double)]
bruteList = [(x,y,z) |  x <- [0..purchasedNumber], -- Can be optimize ceil(amt$/per$)
                        y <- [0..purchasedNumber],
                        z <- [0..purchasedNumber],
                        purchasedNumber == x + y + z,
                        purchasedAmount == sum (zipWith (*) [x,y,z] priceList)
             ]
bruteFan :: [[Double]]
bruteFan = bruteFan' $ replicate (length priceList) 0
    where bruteFan' :: [Double] -> [[Double]]
          bruteFan' cur
            | sum cur > purchasedNumber = []
            | purchasedAmount == sum (zipWith (*) cur priceList) = [cur]
            | otherwise = nub $ concatMap bruteFan' (inc cur)

inc :: [Double] -> [[Double]]
inc xs = inc' (length xs-1) xs
    where inc' :: Int -> [Double] -> [[Double]]
          inc' (-1) _ = []
          inc' n ys = (take n ys ++ [(ys!!n)+1] ++ drop (n+1) ys) : (inc' (n-1) ys)

main :: IO ()
main = do
    print bruteList
    print bruteFan
