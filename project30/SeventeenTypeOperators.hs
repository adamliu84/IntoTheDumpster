-- {-# LANGUAGE TypeOperators #-}

type a + b = Either a b
data a :*: b = Pair a b
    deriving Show

exampleType :: Int + String
exampleType = Left 42

exampleData :: Int :*: String
exampleData = Pair 42 "hello"

type f ~> g = forall x. f x -> g x
-- type NaturalTransformation f g = forall x. f x -> g x

naturalTransform :: Maybe ~> []
-- naturalTransform :: NaturalTransformation Maybe []
-- naturalTransform :: Maybe a -> [a]
naturalTransform Nothing  = []
naturalTransform (Just x) = [x]

main :: IO ()
main = do
    print exampleType
    print exampleData
    print (naturalTransform Nothing :: [()])
    print $ naturalTransform @Double (Just 999)

-- <<OUTPUT
-- Left 42
-- Pair 42 "hello"
-- []
-- [999.0]
-- >>
