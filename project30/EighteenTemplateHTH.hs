{-# LANGUAGE TemplateHaskell #-}
module EighteenTemplateHTH where

import Language.Haskell.TH

-- Generate Haskell expressions at compile time.
factorialQ :: Int -> Q Exp
factorialQ n = return $ LitE $ IntegerL $ toInteger $ product [1..n]

-- Top-level declarations like functions, data types, and type classes.
fibNameTemplate = "helloFib"
fibQ :: Int -> Q [Dec]
fibQ n = do
    sequence [genFib i | i <- [0..n]]
  where
    genFib :: Int -> Q Dec
    genFib i = do
      let name = mkName $ fibNameTemplate ++ show i    
      let value = if i <= 1 
                  then return (LitE (IntegerL (toInteger i)))
                  else [| $(varE (mkName $ fibNameTemplate ++ show (i-1))) + 
                          $(varE (mkName $ fibNameTemplate ++ show (i-2))) |]
      funD name [clause [] (normalB value) []]

-- Rename of gen function
genRenamedFunc :: String -> Q [Dec]
genRenamedFunc funcName = do
  x <- newName "_"
  let name = mkName funcName
  body <- [| $(varE x) + 1 |]  -- Use <- to extract the Exp from Q Exp
  return [FunD name [Clause [VarP x] (NormalB body) []]]     

-- Generate Haskell patterns in function definitions, case expressions, etc.
tuplePatQ :: Int -> Q Pat
tuplePatQ n = do
  names <- sequence [newName ("x" ++ show i) | i <- [1..n]]
  return $ TupP $ map VarP names

-- Generate a tuple type (Int, Int, ...) -> Int with n Int elements in the tuple
tupleTypeQ :: Int -> Q Type
tupleTypeQ n = do
  -- Create the Int type
  intType <- [t| Int |]
  
  -- Build the tuple type
  tupleType <- case n of
    0 -> [t| () |]  -- Empty tuple
    1 -> return intType  -- Single Int (no tuple)
    _ -> do
      -- For n >= 2, create a proper tuple type
      let tupleT = TupleT n
      let intTypes = replicate n intType
      return $ foldl AppT tupleT intTypes
  
  -- Create the function type: tuple -> Int
  returnType <- [t| Int |]
  return $ ArrowT `AppT` tupleType `AppT` returnType

-- Construct complex types programmatically
functionTypeQ :: Int -> Q Type
functionTypeQ n = do
  let argTypes = replicate n [t| Int |]
  foldr (\a b -> [t| $a -> $b |]) [t| Int |] argTypes

-- Modify function names according to some pattern
-- NOTE: For Reflection-like concept
prefixFunctions :: String -> [Name] -> Q [Dec]
prefixFunctions prefix funcs = do
  concat <$> mapM (prefixOne prefix) funcs
  where
    prefixOne pre func = do
      info <- reify func
      case info of
        VarI name t _ -> do
          let newName = mkName $ pre ++ nameBase func
          originalE <- [| $(varE func) |]
          return [FunD newName [Clause [] (NormalB originalE) []]]
        _ -> fail $ show func ++ " is not a function"        