{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Server.Web where

import GHC.Generics

import Servant.API
import Servant.Client

import Data.User
import Data.News

import Data.Text
import Data.Vector
import Data.Proxy

import Data.Time.Calendar.OrdinalDate  

data SortBy
  = SBDate           
  | SBAuthor
  | SBCategory
  | SBCountPhoto
  deriving Generic

instance ToHttpApiData SortBy where
  toUrlPiece SBDate = "date"
  toUrlPiece SBAuthor = "author"
  toUrlPiece SBCategory = "category"
  toUrlPiece SBCountPhoto = "photo"
   
type OffSet = Int
type Limit = Int

type API = "get_news" :> "public"
                      :> QueryParams "created_at" Day
                      :> QueryParams "created_until" Day
                      :> QueryParams "created_since" Day
                      :> QueryParams "aothor_name" Text
                      :> QueryParams "category" Text
                      :> QueryParams "news_name" Text
                      :> QueryParams "content" Text
                      :> QueryParams "for_string" Text
                      :> QueryParams "published" Bool
                      :> QueryParams "offset" OffSet
                      :> QueryParams "limit" Limit
                      :> QueryParams "sort_by" SortBy
                      :> Get '[JSON] [News]
      :<|> "get_news" :> "private" 
                      :> BasicAuth "user" UserPublic 
                      :> QueryParams "created_at" Day
                      :> QueryParams "created_until" Day
                      :> QueryParams "created_since" Day
                      :> QueryParams "aothor_name" Text
                      :> QueryParams "category" Text
                      :> QueryParams "news_name" Text
                      :> QueryParams "content" Text
                      :> QueryParams "for_string" Text
                      :> QueryParams "sort_by" SortBy
                      :> QueryParams "published" Bool
                      :> QueryParams "offset" OffSet
                      :> QueryParams "limit" Limit
                      :> Get '[JSON] [News] 
      :<|> "category" :> "create" :> BasicAuth "user" UserPublic -- realms ???
                                  :> QueryParams "root" Text
                                  :> QueryParams "category_name" Text
                                  :> Get '[JSON] NewsCategory 
      :<|> "category" :> "get_tree" :> Get '[JSON] NewsCategory
      :<|> "category" :>  "change" :> BasicAuth "user" UserPublic
                                   :> QueryParams "category_name" Text
                                   :> QueryParams "new_root" Text
                                   :> QueryParams "new_name" Text
                                   :> Get '[JSON] NewsCategory
      :<|> "create_news" :> "new" :> BasicAuth "user" UserPublic
                                  :> ReqBody '[JSON] NewsCreate
                                  :> Get '[JSON] News
      :<|> "create_news" :> "edit" :> BasicAuth "user" UserPublic
                                   :> QueryParams "news_name" Text
                                   :> QueryParams "text" Text
                                   :> QueryParams "news_new_name" Text
                                   :> QueryParams "category" Text
                                   :> QueryParams "public" Bool
                                   :> ReqBody' '[Optional, Strict] '[JSON] (Vector PhotoURL)
                                   :> Get '[JSON] News
      :<|> "user" :> "create" :> BasicAuth "user" UserPublic
                              :> QueryParams "name" Text
                              :> QueryParams "login" Text
                              :> QueryParams "password" Text
                              :> QueryParams "make_news" Bool
                              :> QueryParams "admin" Bool
                              :> Get '[JSON] UserPublic
      :<|> "user" :> "list" :> QueryParams "offset" OffSet
                            :> QueryParams "limit" Limit
                            :> Get '[JSON] [UserPublic]
                    
      -- :<|> "autorization" :> QueryParams "login" :> QueryParams "password" :> Get '[JSON] UserPublic

api :: Proxy API
api = Proxy

getNewsPublic :: Maybe Day 
              -> Maybe Day 
              -> Maybe Day 
              -> Maybe Text    
              -> Maybe Text
              -> Maybe Text
              -> Maybe Text
              -> Maybe Text
              -> Maybe Bool
              -> Maybe OffSet
              -> Maybe Limit
              -> Maybe SortBy
              -> ClientM [News]
getNewsPrivate :: BasicAuthData
               -> Maybe Day 
               -> Maybe Day 
               -> Maybe Day 
               -> Maybe Text    
               -> Maybe Text
               -> Maybe Text
               -> Maybe Text
               -> Maybe Text
               -> Maybe Bool
               -> Maybe OffSet
               -> Maybe Limit
               -> Maybe SortBy
               -> ClientM [News]
categoryCreate :: BasicAuthData -> Maybe Text -> Maybe Text -> ClientM NewsCategory
categoryGetTree :: ClientM NewsCategory
categoryChange :: BasicAuthData -> Maybe Text -> Maybe Text -> Maybe Text -> ClientM NewsCategory
createNewsNew :: BasicAuthData -> NewsCreate -> ClientM News
createNewsEdit :: BasicAuthData 
               -> Maybe Text   
               -> Maybe Text
               -> Maybe Text
               -> Maybe Text
               -> Maybe Bool
               -> Vector PhotoURL
               -> ClientM News
userCreate :: BasicAuthData 
           -> Maybe Text
           -> Maybe Text
           -> Maybe Text
           -> Maybe Bool
           -> Maybe Bool
           -> ClientM UserPublic
userList :: Maybe OffSet -> [Limit] -> ClientM [UserPublic]

getNewsPublic :<|> getNewsPrivate :<|> categoryCreate :<|> categoryGetTree :<|> categoryChange :<|> createNewsNew :<|> createNewsEdit :<|> userCreate :<|> userList = client api
