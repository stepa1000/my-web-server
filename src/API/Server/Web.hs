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
import Data.Types

import Data.Text
import Data.Vector
import Data.Proxy

import Data.Time.Calendar.OrdinalDate  

{- 
type OffSet = Int
type Limit = Int

type DayAt = Day
type DayUntil = Day
type DaySince = Day

type Aothor = Text
type Category = Text
type NewsName = Text
type Content = Text

type ForString = Text
type FlagPublished = Bool
-}
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
                                   :> QueryParam' '[Strict] "category_name" Text
                                   :> QueryParam "new_root" Text
                                   :> QueryParam "new_name" Text
                                   :> Get '[JSON] NewsCategory
      :<|> "create_news" :> "new" :> BasicAuth "user" UserPublic
                                  :> ReqBody '[JSON] NewsCreate
                                  :> Get '[JSON] News
      :<|> "create_news" :> "edit" :> BasicAuth "user" UserPublic
                                   :> QueryParam "news_name" NameNews
                                   :> QueryParam "text" Content
                                   :> QueryParam "news_new_name" NameNews
                                   :> QueryParam "category" Content
                                   :> QueryParam "public" FlagPublished
                                   :> ReqBody' '[Optional, Strict] '[JSON] (Vector PhotoURL)
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
createNewsNew :: BasicAuthData -> NewsCreate -> ClientM News
createNewsEdit :: BasicAuthData 
               -> Maybe NameNews -- old   
               -> Maybe Content
               -> Maybe NameNews -- new
               -> Maybe Content
               -> Maybe FlagPublished
               -> Vector PhotoURL
               -> ClientM News
userCreate :: BasicAuthData 
           -> Maybe Name
           -> Maybe Login
           -> Maybe Password
           -> Maybe FlagMakeNews
           -> Maybe FlagAdmin
           -> ClientM UserPublic
userList :: Maybe OffSet -> Maybe Limit -> ClientM [UserPublic]

getNewsPublic :<|> getNewsPrivate :<|> categoryCreate :<|> categoryGetTree :<|> categoryChange :<|> createNewsNew :<|> createNewsEdit :<|> userCreate :<|> userList = client api
