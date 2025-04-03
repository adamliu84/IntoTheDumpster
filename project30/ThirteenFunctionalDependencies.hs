{-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}

class Collection c e | c -> e where
  insert :: e -> c -> c
  lookup :: e -> c -> Maybe e

data MySet a = MySet [a] deriving (Show)

instance Eq a => Collection (MySet a) a where
  insert :: Eq a => a -> MySet a -> MySet a
  insert x (MySet xs) = MySet (x : xs)
  lookup :: Eq a => a -> MySet a -> Maybe a
  lookup x (MySet xs) = find (== x) xs
    where
      find p [] = Nothing
      find p (y:ys)
        | p y = Just y
        | otherwise = find p ys

main :: IO ()
main = do    
    let a = MySet [1..10]
        a' = Main.insert 99 a
    print a
    print a'
    print $ Main.lookup 99 a'
    print $ Main.lookup 999 a'

-- << OUTPUT
-- MySet [1,2,3,4,5,6,7,8,9,10]
-- MySet [99,1,2,3,4,5,6,7,8,9,10]
-- Just 99
-- Nothing
-- >>