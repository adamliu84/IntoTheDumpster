{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Kind (Type)

data Nat = Zero | Succ Nat

data ValueHolder (n :: Nat) where
  VHInt  :: ValueHolder 'Zero
  VHBool :: ValueHolder ('Succ 'Zero)

describeValue :: ValueHolder n -> String
describeValue VHInt = "This holds an Int."
describeValue VHBool = "This holds a Bool."

main :: IO ()
main = do
  let myIntHolder = VHInt :: ValueHolder 'Zero
  let myBoolHolder = VHBool :: ValueHolder ('Succ 'Zero)  
  putStrLn $ describeValue myIntHolder
  putStrLn $ describeValue myBoolHolder

-- << OUTPUT
-- This holds an Int.
-- This holds a Bool.
-- >>