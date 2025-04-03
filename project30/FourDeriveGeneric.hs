-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           GHC.Generics (Generic)

data Person = Person
  { name    :: String
  , age     :: Int
  , address :: Address
  } deriving (Show, Generic)

data Address = Address
  { street  :: String
  , city    :: String
  , zipCode :: String
  } deriving (Show, Generic)

instance ToJSON Person
instance FromJSON Person
instance ToJSON Address
instance FromJSON Address

-- NOTE
-- instance ToJSON Employee where
--   toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 3 } 

-- data Person = Person
--   { name    :: String
--   , age     :: Int
--   , address :: Address
--   } deriving (Show)

-- data Address = Address
--   { street  :: String
--   , city    :: String
--   , zipCode :: String
--   } deriving (Show)

-- instance ToJSON Person where
--   toJSON (Person name age addr) = object
--     [ "name" .= name
--     , "age" .= age
--     , "address" .= addr    
--     ]
-- instance FromJSON Person where
--   parseJSON (Object v) = Person
--     <$> v .: "name"
--     <*> v .: "age"
--     <*> v .: "address"    
--   parseJSON _ = fail "Expected an object for Person"
-- instance ToJSON Address where
--   toJSON (Address street city zip) = object
--     [ "street" .= street
--     , "city" .= city
--     , "zipCode" .= zip
--     ]
-- instance FromJSON Address where
--   parseJSON (Object v) = Address
--     <$> v .: "street"
--     <*> v .: "city"
--     <*> v .: "zipCode"
--   parseJSON _ = fail "Expected an object for Address"

main :: IO ()
main = do
  let john = Person "John Doe" 30 (Address "Wall Street" "NY" "998877")
  putStrLn $ "JSON: " ++ show (encode john)
  putStrLn $ "Address: " ++ show (decode "{\"street\":\"Dotonbori\",\"city\":\"Osaka\",\"zipCode\":\"112233\"}" :: Maybe Address)

-- << OUTPUT
-- JSON: "{\"address\":{\"city\":\"NY\",\"street\":\"Wall Street\",\"zipCode\":\"998877\"},\"age\":30,\"name\":\"John Doe\"}"
-- Address: Just (Address {street = "Dotonbori", city = "Osaka", zipCode = "112233"})
-- >>