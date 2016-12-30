{-# LANGUAGE OverloadedStrings #-}
module Vk where

import Prelude ()
import Prelude.Compat

import Network.Wreq (post, Response, FormParam((:=)), responseBody)

import Data.ByteString.Lazy (ByteString)

import Control.Lens ((^.))

import Data.Aeson (FromJSON(..), fromJSON, Value(..), (.:), (.:?), decode)
import Data.Aeson.Types (Result(..))

data Profile = Profile { firstName :: String, lastName :: String, uid :: Int }
  deriving (Show)

instance FromJSON Profile where
  parseJSON (Object v) =
    Profile <$> v .: "first_name"
            <*> v .: "last_name"
            <*> v .: "uid"

type APIErr = Value

newtype APIResp a = APIResp (Either APIErr a) deriving (Show)

instance (FromJSON a) => FromJSON (APIResp a) where
  parseJSON (Object v) = do
    x <- v .:? "response"
    case x of
      Just y  -> case fromJSON y of
        Success z -> return $ APIResp $ Right z
        Error   z -> error z
      Nothing -> APIResp <$> Left <$> v .: "error"
  parseJSON _ = fail "Expecting Object"

getFriends :: Profile -> IO [Profile]
getFriends (Profile { uid = x }) = do
  r <- post "https://api.vk.com/method/friends.get"
    ["user_id" := x,
     "fields" := ("first_name,last_name" :: String),
     "lang" := ("en" :: String)]
  let resp = decode $ r ^. responseBody :: Maybe (APIResp [Profile])
  case resp of
    Just (APIResp (Right x)) -> return x
    Just (APIResp (Left  x)) -> fail $ "API error: " ++ show x
    _ -> undefined

-- TODO: Move duplicate code from get{Friends, User} to a separate function
getUser :: Int -> IO Profile
getUser x = do
  r <- post "https://api.vk.com/method/users.get"
    ["user_ids" := x,
     "fields" := ("first_name,last_name" :: String),
     "lang" := ("en" :: String)]
  let resp = decode $ r ^. responseBody :: Maybe (APIResp [Profile])
  case resp of
    Just (APIResp (Right x)) -> return $ head x
    Just (APIResp (Left  x)) -> fail $ "API error: " ++ show x
    _ -> undefined
