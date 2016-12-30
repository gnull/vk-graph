{-# LANGUAGE OverloadedStrings #-}
module Vk where

import Prelude ()
import Prelude.Compat

import Network.Wreq (post, Response, FormParam((:=)), responseBody)

import Data.ByteString.Lazy (ByteString)

import Control.Lens ((^.))

data Profile = Profile { firstName :: String, lastName :: String, uid :: Int }
  deriving (Show)

getFriends :: String -> IO ByteString
getFriends x = do
  r <- post "https://api.vk.com/method/friends.get"
    ["user_id" := x,
     "fields" := ("first_name,last_name" :: String),
     "lang" := ("en" :: String)]
  return $ r ^. responseBody
