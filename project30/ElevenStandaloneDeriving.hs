
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-
ElevenStandaloneDeriving.hs:20:1: error: [GHC-00158]
    • Can't make a derived instance of ‘Custom Shape’:
        ‘Custom’ is not a stock derivable class (Eq, Show, etc.)
    • In the stand-alone deriving instance for ‘Custom Shape’
    Suggested fix: Perhaps you intended to use DeriveAnyClass
-}

newtype UserID = UserID Int
deriving instance Show UserID

class Custom a where
  custom :: a -> String
  custom _ = "default..."

data Shape = Circle | Square
deriving instance Custom Shape

main :: IO ()
main = do
    print (UserID 42)
    print $ custom Circle

-- << OUTPUT
-- UserID 42
-- "default..."
-- >>