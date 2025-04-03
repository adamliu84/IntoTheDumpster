-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-} -- Silence warning: [GHC-62412] [-Wsimplifiable-class-constraints]

printMaybe :: Show (Maybe a) => Maybe a -> IO ()
printMaybe = print

main :: IO ()
main = do
  printMaybe (Just 42)
  printMaybe (Just "Hi")  
  printMaybe (Nothing :: Maybe ())

-- << OUTPUT
-- Just 42
-- Just "Hi"
-- Nothing
-- >>