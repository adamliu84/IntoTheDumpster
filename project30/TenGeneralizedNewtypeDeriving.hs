-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Dollars a = Dollars a
  deriving (Eq, Ord, Num)

-- Manual instance of Show
instance Show a => Show (Dollars a) where
    show :: Dollars a -> String
    show (Dollars n) = show n

sumUp :: Dollars Int
sumUp = Dollars 10 + Dollars 5

compareAgainst :: Bool -- (>) :: a -> a -> Bool 
compareAgainst = Dollars 'A' > Dollars 'B'

maxBetween :: Dollars Char -- max :: a -> a -> a
maxBetween = Dollars 'A' `max` Dollars 'B'

main :: IO ()
main = do
    print sumUp
    print compareAgainst
    print maxBetween

-- << OUTPUT
-- 15
-- False
-- 'B'
-- >>