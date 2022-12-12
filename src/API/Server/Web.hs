module API.Server.Web where

import Data.User
import Data.News

import Data.Time.Calendar.OrdinalDate  

type API = "news" :> QueryParams "created_at" Day
                  :> QueryParams "created_until" Day
                  :> QueryParams "created_since" Day
                  :> QueryParams "aothor_name" Text
                  :> QueryParams "category" Text
                  :> QueryParams "news_name" Text
                  :> QueryParams "content" Text
                  :> QueryParams "for_string" Text
                  :> QueryParams "sort_by" SortBy
                  :> Get '[JSON] [News]
      :<|> "category" :> ( "create" :> BasicAuth "admin" NewsCategory :> Get '[JSON] NewsCategory 
                         )    
      :<|> "autorization" :> QueryParams "login" :> QueryParams "password" :> Get '[JSON] UserPublic
