{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Server.Web  
  ( basicAuthDataToLogined
  , API
  , api
  , getNewsPublic
  , getNewsPrivate
  , categoryCreate
  , categoryGetTree
  , categoryChange
  , createNewsNew
  , createNewsEdit
  , userCreate
  , userList
  , photoGet
  ) where

-- import GHC.Generics

import Servant.API
import Servant.Client

import Data.User
import Data.News
import Data.Types

-- import Data.Text
import Data.Text.Encoding
-- import Data.ByteString
-- import Data.Vector

import Data.Proxy

-- import Data.Time.Calendar.OrdinalDate  

basicAuthDataToLogined :: BasicAuthData -> Logined
basicAuthDataToLogined bad = Logined
  { loginLogined = decodeUtf8 $ basicAuthUsername bad -- decodeLatin1 $ basicAuthUsername bad -- decodeUtf8Lenient $ basicAuthUsername bad
  , passwordLogined = decodeUtf8 $ basicAuthPassword bad -- decodeLatin1 $ basicAuthPassword bad -- decodeUtf8Lenient $ basicAuthPassword bad
  }

type API = "get_news" :> "public"
                      :> QueryParam "created_at" DayAt
                      :> QueryParam "created_until" DayUntil
                      :> QueryParam "created_since" DaySince
                      :> QueryParam "aothor_name" Name
                      :> QueryParam "category" Category
                      :> QueryParam "news_name" NewsName
                      :> QueryParam "content" Content
                      :> QueryParam "for_string" ForString
                      -- :> QueryParams "published" FlagPublished
                      :> QueryParam "sort_by" SortBy
                      :> QueryParam "offset" OffSet
                      :> QueryParam "limit" Limit
                      :> Get '[JSON] [News]
      :<|> "get_news" :> "private" 
                      :> BasicAuth "user" UserPublic 
                      :> QueryParam "created_at" DayAt
                      :> QueryParam "created_until" DayUntil
                      :> QueryParam "created_since" DaySince
                      :> QueryParam "aothor_name" Name
                      :> QueryParam "category" Category
                      :> QueryParam "news_name" NewsName
                      :> QueryParam "content" Content
                      :> QueryParam "for_string" ForString
                      :> QueryParam "published" FlagPublished
                      :> QueryParam "sort_by" SortBy
                      :> QueryParam "offset" OffSet
                      :> QueryParam "limit" Limit
                      :> Get '[JSON] [News] 
      :<|> "category" :> "create" :> BasicAuth "user" UserPublic -- realms ???
                                  :> QueryParam "root" Category
                                  :> QueryParam' '[Strict] "category_name" Category
                                  :> Get '[JSON] NewsCategory 
      :<|> "category" :> "get_tree" :> Get '[JSON] NewsCategory
      :<|> "category" :>  "change" :> BasicAuth "user" UserPublic
                                   :> QueryParam' '[Strict] "category_name" Category
                                   :> QueryParam "new_root" Category
                                   :> QueryParam "new_name" Category
                                   :> Get '[JSON] NewsCategory
      :<|> "create_news" :> "new" :> BasicAuth "user" UserPublic
                                  :> ReqBody '[JSON] NewsCreate
                                  -- :> QueryParams "new_photos" ByteString
                                  :> Get '[JSON] News
      :<|> "create_news" :> "edit" :> BasicAuth "user" UserPublic
                                   :> QueryParam "news_name" NameNews
                                   :> QueryParam "text" Content
                                   :> QueryParam "news_new_name" NameNews
                                   :> QueryParam "category" Category
                                   -- :> QueryParams "photos_url" PhotoURL
                                   -- :> QueryParams "new_photos" ByteString
                                   :> QueryParam "public" FlagPublished
                                   :> QueryParams "photos" Photo
                                   :> QueryParams "new_photos" Base64
                                   :> Get '[JSON] News
      :<|> "user" :> "create" :> BasicAuth "user" UserPublic
                              :> QueryParam "name" Name
                              :> QueryParam "login" Login
                              :> QueryParam "password" Password
                              :> QueryParam "make_news" FlagMakeNews
                              :> QueryParam "admin" FlagAdmin
                              :> Get '[JSON] UserPublic
      :<|> "user" :> "list" :> QueryParam "offset" OffSet
                            :> QueryParam "limit" Limit
                            :> Get '[JSON] [UserPublic]
      :<|> "photo" :> "get" :> QueryParam "name_photo" Photo :> Get '[JSON] Base64 -- ByteString
                    
      -- :<|> "autorization" :> QueryParams "login" :> QueryParams "password" :> Get '[JSON] UserPublic

api :: Proxy API
api = Proxy

getNewsPublic :: Maybe DayAt
              -> Maybe DayUntil 
              -> Maybe DaySince 
              -> Maybe Name    
              -> Maybe Category
              -> Maybe NewsName
              -> Maybe Content
              -> Maybe ForString
              -- -> [FlagPublished]
              -> Maybe SortBy    
              -> Maybe OffSet
              -> Maybe Limit
              -> ClientM [News]
getNewsPrivate :: BasicAuthData
               -> Maybe DayAt 
               -> Maybe DayUntil 
               -> Maybe DaySince 
               -> Maybe Name    
               -> Maybe Category
               -> Maybe NewsName
               -> Maybe Content
               -> Maybe ForString
               -> Maybe FlagPublished
               -> Maybe SortBy    
               -> Maybe OffSet
               -> Maybe Limit
               -> ClientM [News]
categoryCreate :: BasicAuthData -> Maybe Category -> Maybe Category -> ClientM NewsCategory
categoryGetTree :: ClientM NewsCategory
categoryChange :: BasicAuthData -> Maybe Category -> Maybe Category -> Maybe Category -> ClientM NewsCategory
createNewsNew :: BasicAuthData -> NewsCreate {- > [ByteString]-} -> ClientM News
createNewsEdit :: BasicAuthData 
               -> Maybe NameNews -- old   
               -> Maybe Content
               -> Maybe NameNews -- new
               -> Maybe Content
               -> Maybe FlagPublished
               -> [Photo]
               -> [Base64] -- ByteString
               -> ClientM News
userCreate :: BasicAuthData 
           -> Maybe Name
           -> Maybe Login
           -> Maybe Password
           -> Maybe FlagMakeNews
           -> Maybe FlagAdmin
           -> ClientM UserPublic
userList :: Maybe OffSet -> Maybe Limit -> ClientM [UserPublic]
photoGet :: Maybe Photo -> ClientM Base64 -- ByteString

getNewsPublic :<|> getNewsPrivate :<|> categoryCreate :<|> categoryGetTree :<|> categoryChange :<|> createNewsNew :<|> createNewsEdit :<|> userCreate :<|> userList :<|> photoGet= client api
