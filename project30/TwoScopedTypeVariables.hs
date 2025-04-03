-- {-# LANGUAGE ScopedTypeVariables #-}

identity' :: forall a. a -> a
identity' x = y
    where
        y :: a -- 'a' refers to the same type as in the outer signature
        y = x

identity'' :: (forall a. a -> a) -> a -> b -> (a, b)
identity'' f x y = (f x, f y)

-- Rank-2 sample
higherRank :: (forall a. a -> a) -> (Int, String, [Int])
higherRank f = (f 42, f "hello", f [1..10])

-- Rank-2 with constraints
higherRankShow :: Show a  => (forall z. Show z => z -> String) -> a -> String
higherRankShow f = f

main = do
    print $ identity' 99
    print $ identity'' id "Hello" 111
    print $ higherRank id
    print $ higherRankShow show [1..10]

-- << OUTPUT
-- 99
-- ("Hello",111)
-- (42,"hello",[1,2,3,4,5,6,7,8,9,10])
-- "[1,2,3,4,5,6,7,8,9,10]"
-- >>
