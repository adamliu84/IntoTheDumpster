{-
Credit take reference from
https://github.com/tzemanovic/haskell-yesod-realworld-example-app/blob/b0eb3bdc6b64358d59a4d5241283afeb8b530184/app/src/Auth/JWT.hs
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth.JWT
  ( lookupToken
  , jsonToToken
  , tokenToJson
  , tokenToExp
  , tokenToUserIdWithExpCheck
  , UserId
  )
  where

import           ClassyPrelude.Yesod
import           Data.Char           (isSpace)
import           Data.Map             as Map (fromList, (!?))
import           Web.JWT              as JWT
import           Data.Time.Clock

type UserId = String

-- | Try to lookup token from the Authorization header
lookupToken :: MonadHandler m => m (Maybe Text)
lookupToken = do
  mAuth <- lookupHeader "Authorization"
  return $ extractToken . decodeUtf8 =<< mAuth

-- | Create a token out of a given JSON 'Value'
jsonToToken :: NominalDiffTime -> Text -> Value -> Text
jsonToToken expTime jwtSecret userId =
  encodeSigned (JWT.hmacSecret jwtSecret) mempty
    mempty {unregisteredClaims = ClaimsMap $ Map.fromList [(jwtKey, userId)], JWT.exp = numericDate expTime}

-- | Extract a JSON 'Value' out of a token
tokenToUserIdWithExpCheck :: NominalDiffTime -> Text -> Text -> Maybe Value
tokenToUserIdWithExpCheck curTime jwtSecret token = do
  expUser <- tokenToExpUserJson jwtSecret token
  case (curTime < fst expUser) of
    True  -> return (snd expUser)
    False -> Nothing

tokenToExp :: Text -> Text -> Maybe NominalDiffTime
tokenToExp jwtSecret token = fst <$> tokenToExpUserJson jwtSecret token

tokenToJson :: Text -> Text -> Maybe Value
tokenToJson jwtSecret token = snd <$> tokenToExpUserJson jwtSecret token

tokenToExpUserJson :: Text -> Text -> Maybe (NominalDiffTime, Value)
tokenToExpUserJson jwtSecret token = do
    jwt <- JWT.decodeAndVerifySignature (JWT.hmacSecret jwtSecret) token
    expUserChecker
      (JWT.exp $ JWT.claims jwt)
      (unClaimsMap (JWT.unregisteredClaims (JWT.claims jwt)) !? jwtKey)
    where expUserChecker (Just ev) (Just uv) = Just (secondsSinceEpoch ev, uv)
          expUserChecker _         _         = Nothing

jwtKey :: Text
jwtKey = "jwt"

extractToken :: Text -> Maybe Text
extractToken auth
  | toLower x == "bearer" = Just $ dropWhile isSpace y
  | otherwise            = Nothing
  where (x, y) = break isSpace auth