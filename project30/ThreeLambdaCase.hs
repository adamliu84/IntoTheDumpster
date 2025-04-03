{-# LANGUAGE LambdaCase #-}
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/lambda_case.html

process :: Maybe Int -> String
process = \case
  Just n  -> "Value: " ++ show n
  Nothing -> "No value"

main :: IO ()
main = do
    print . process        $ Nothing
    print . process . Just $ 99

-- << OUTPUT
-- "No value"
-- "Value: 99"
-- >>
