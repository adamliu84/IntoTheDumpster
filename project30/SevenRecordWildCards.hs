{-# LANGUAGE RecordWildCards #-}

data Point = Point { x :: Int, y :: Int, z :: Int }

processPoint :: Point -> String
processPoint Point { .. } = "X: " ++ show x ++ ", Y: " ++ show y ++ ", Z: " ++ show z

main :: IO ()
main = do
    print $ processPoint $ Point 10 20 30

-- << OUTPUT
-- "X: 10, Y: 20, Z: 30"
-- >>