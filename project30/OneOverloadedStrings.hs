{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

greet :: T.Text -> T.Text
greet name = "Hello, " <> name

main :: IO ()
main = print $ greet "World"

-- << OUTPUT
-- vscode ➜ ~/extension $ stack runhaskell OneOverloadedStrings.hs
-- "Hello, World"
-- >>
