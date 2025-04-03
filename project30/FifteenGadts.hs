{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE MonoLocalBinds #-}

data Expr a where
  Lit :: Int -> Expr Int
  Add :: Expr Int -> Expr Int -> Expr Int
  Flag :: Bool -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

eval :: Expr a -> a
eval (Lit n) = n
eval (Add x y) = eval x + eval y
eval (Flag n) = n
eval (If cond thenExpr elseExpr) =
  if eval cond then eval thenExpr else eval elseExpr

main :: IO ()
main = do
    print $ eval (Add (Lit 4) (Lit 5))
    print $ eval (If (Flag False) 
                        (Add (Lit 4) (Lit 5))
                        (Add (Lit 200) (Lit 100))
                 )
    print $ eval (If (Flag True)      -- Deliberately "NOT" logic
                        (Flag False)
                        (Flag True)
                 )

-- << OUTPUT
-- 9
-- 300
-- False
-- >>