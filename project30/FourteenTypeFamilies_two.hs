{-# LANGUAGE TypeFamilies #-}

import           Data.List (intercalate)

class Collection c where
  type Elem c
  empty :: c
  insert :: Elem c -> c -> c
  toList :: c -> [Elem c]

instance Collection [a] where
  type Elem [a] = a
  empty :: [a]
  empty = []
  insert :: Elem [a] -> [a] -> [a]
  insert x xs = x:xs
  -- PERSONAL NOTE: toList :: [a] -> [a] <=> toList :: [a] -> [Elem [a]]
  -- Just swap in Elem [a] == a
  -- This is just a "type"...
  toList :: [a] -> [Elem [a]]
  toList = id

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

instance Ord a => Collection (Tree a) where
  type Elem (Tree a) = a
  empty :: Ord a => Tree a
  empty = Empty
  insert :: Ord a => Elem (Tree a) -> Tree a -> Tree a
  insert x Empty = Node x Empty Empty
  insert x (Node y left right)
    | x < y     = Node y (insert x left) right
    | otherwise = Node y left (insert x right)
  toList :: Ord a => Tree a -> [Elem (Tree a)]
  toList Empty               = []
  toList (Node x left right) = toList left ++ [x] ++ toList right

data Pair a = Pair a a deriving (Show)

instance Collection (Pair a) where
  type Elem (Pair a) = a
  empty :: Pair a
  empty = error "Cannot create an empty Pair"
  insert :: Elem (Pair a) -> Pair a -> Pair a
  insert x (Pair a _) = Pair a x
  toList :: Pair a -> [Elem (Pair a)]
  toList (Pair a b) = [a, b]

newtype StringMap a = StringMap [(String, a)] deriving (Show)

instance Collection (StringMap a) where
  type Elem (StringMap a) = (String, a)
  empty :: StringMap a
  empty = StringMap []
  insert :: Elem (StringMap a) -> StringMap a -> StringMap a
  insert (k, v) (StringMap kvs) = StringMap ((k, v) : filter (\(k', _) -> k' /= k) kvs)
  -- PERSONAL NOTE: toList :: StringMap a -> [(String, a)] <=> toList :: StringMap a -> [Elem (StringMap a)]
  -- Just swap in Elem (StringMap a) == String, a)
  toList :: StringMap a -> [Elem (StringMap a)]
  toList (StringMap kvs) = kvs

addElements :: (Collection c) => [Elem c] -> c -> c
addElements xs coll = foldr insert coll xs

showCollection :: (Collection c, Show (Elem c)) => c -> String
showCollection coll = "[" ++ intercalate ", " (map show (toList coll)) ++ "]"

main :: IO ()
main = do
  let intList = addElements [5, 3, 8, 1] empty :: [Int]
  putStrLn $ "Int List: " ++ showCollection intList

  let intTree = addElements [5, 3, 8, 1] empty :: Tree Int
  putStrLn $ "Int Tree: " ++ showCollection intTree

  let pair = insert 42 (Pair 10 20)
  putStrLn $ "Pair: " ++ showCollection pair

  let map = addElements [("name", "Haskell"), ("year", "1990")] empty :: StringMap String
  putStrLn $ "StringMap: " ++ showCollection map

-- << OUTPUT
-- Int List: [5, 3, 8, 1]
-- Int Tree: [1, 3, 5, 8]
-- Pair: [10, 42]
-- StringMap: [("name","Haskell"), ("year","1990")]
-- >>
