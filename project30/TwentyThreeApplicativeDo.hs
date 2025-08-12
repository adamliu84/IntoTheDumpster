{-# LANGUAGE ApplicativeDo #-}

import Control.Applicative

newtype Logger a = Logger { runLogger :: ([String], a) }
    deriving Show

instance Functor Logger where
    fmap f (Logger (log, x)) = Logger (log, f x)

instance Applicative Logger where
    pure x = Logger ([], x)
    Logger (log1, f) <*> Logger (log2, x) = Logger (log1 ++ log2, f x)

-- instance Monad Logger where
--     Logger (log1, x) >>= f =
--         let Logger (log2, y) = f x
--         in Logger (log1 ++ log2, y)

logValue :: String -> Logger String
logValue s = Logger ([s], s)

main :: IO ()
main = do
    let result = do
            x <- logValue "First"
            y <- logValue "Second"
            pure (x, y)
    print (runLogger result)

-- << OUTPUT
-- (["First","Second"],("First","Second"))
-- >>