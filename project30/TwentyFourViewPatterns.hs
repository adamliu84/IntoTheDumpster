{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Char (isDigit)

data User = User
  { name    :: String
  , age     :: Int
  , userId  :: String
  } deriving Show

isDigits :: String -> Bool
isDigits = all isDigit


-- checkIdValidity :: User -> String
-- checkIdValidity user =
--   if isDigits (userId user)
--   then "User ID is numeric."
--   else "User ID is not numeric."
checkIdValidity :: User -> String
checkIdValidity (userId -> isDigits -> True) = "User ID is numeric."
checkIdValidity (userId -> isDigits -> False) = "User ID is not numeric."

main :: IO ()
main = do
  let user1 = User "Alice" 30 "12345"
  let user2 = User "Bob" 25 "bob123"

  putStrLn $ "User 1 ID validity: " ++ checkIdValidity user1
  putStrLn $ "User 2 ID validity: " ++ checkIdValidity user2

-- << OUTPUT
-- User 1 ID validity: User ID is numeric.
-- User 2 ID validity: User ID is not numeric.
-- >>