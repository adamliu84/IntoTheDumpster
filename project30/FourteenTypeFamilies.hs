{-# LANGUAGE TypeFamilies #-}

newtype Custom a = Custom a

type family ElementType container where
  ElementType [a] = a
  ElementType (Maybe a) = a
  ElementType (Either a b) = b
  ElementType (Custom _) = String -- For example, force return String

-- class HasElement container where
--   firstElement :: container -> ElementType container
class HasElement c where
  firstElement :: c -> ElementType c

instance HasElement [a] where
  firstElement :: [a] -> ElementType [a]
  firstElement []    = error "Empty list"
  firstElement (x:_) = x

instance HasElement (Maybe a) where
  firstElement :: Maybe a -> ElementType (Maybe a)
  firstElement Nothing  = error "Nothing has no elements"
  firstElement (Just x) = x

instance HasElement (Either a b) where
  firstElement :: Either a b -> ElementType (Either a b)
  firstElement (Left _)  = error "Cannot get element from Left"
  firstElement (Right x) = x

instance HasElement (Custom a) where
    firstElement :: Custom a -> ElementType (Custom a)
    firstElement _ = "Hello World"

main :: IO ()
main = do
  let list = [10, 20, 30]
  let maybeValue = Just "hello"
  let eitherValue = Right True :: Either String Bool
  let customValue = Custom 444
  putStrLn $ "First from list: " ++ show (firstElement list)
  putStrLn $ "First from Maybe: " ++ show (firstElement maybeValue)
  putStrLn $ "First from Either: " ++ show (firstElement eitherValue)
  putStrLn $ "First from Custom: " ++ show (firstElement customValue)

-- << OUTPUT
-- First from list: 10
-- First from Maybe: "hello"
-- First from Either: True
-- First from Custom: "Hello World"
-- >>
