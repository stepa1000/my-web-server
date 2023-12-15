{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Server.Web
  ( basicAuthDataToLogined,
    loginedToBasicAuthData,
    API,
    api,
    getNewsPublic,
    getNewsPrivate,
    categoryCreate,
    categoryGetTree,
    categoryChange,
    createNewsNew,
    createNewsEdit,
    userCreate,
    userList,
    photoGet,
    GetNewsPublic,
    GetNewsPrivate,
    CategoryCreate,
    CategoryChange,
    CreateNewsEdit,
    UserCreate,
    UserList,
    CategoryGetTree,
    CreateNewsNew,
    PhotoGet,
  )
where

import Data.News
import Data.Proxy
import Data.Text.Encoding
import Data.Types
import Data.UUID
import Data.User
import Servant.API
import Servant.Client

basicAuthDataToLogined :: BasicAuthData -> Logined
basicAuthDataToLogined bad =
  Logined
    { loginLogined = decodeUtf8 $ basicAuthUsername bad,
      passwordLogined = decodeUtf8 $ basicAuthPassword bad
    }

loginedToBasicAuthData :: Logined -> BasicAuthData
loginedToBasicAuthData l =
  BasicAuthData
    { basicAuthUsername = encodeUtf8 $ loginLogined l,
      basicAuthPassword = encodeUtf8 $ passwordLogined l
    }

-- | Server API.
type API =
  "get_news"
    :> "public"
    :> QueryParam "created_at" DayAt
    :> QueryParam "created_until" DayUntil
    :> QueryParam "created_since" DaySince
    :> QueryParam "aothor_name" Name
    :> QueryParam "category" Category
    :> QueryParam "news_uuid" UUID
    :> QueryParam "news_name" NewsName
    :> QueryParam "content" Content
    :> QueryParam "for_string" ForString
    :> QueryParam "sort_by" SortBy
    :> QueryParam "offset" OffSet
    :> QueryParam "limit" Limit
    :> Get '[JSON] [News]
    :<|> "get_news"
      :> "private"
      :> BasicAuth "user" UserPublic
      :> QueryParam "created_at" DayAt
      :> QueryParam "created_until" DayUntil
      :> QueryParam "created_since" DaySince
      :> QueryParam "category" Category
      :> QueryParam "news_uuid" UUID
      :> QueryParam "news_name" NewsName
      :> QueryParam "content" Content
      :> QueryParam "for_string" ForString
      :> QueryParam "published" FlagPublished
      :> QueryParam "sort_by" SortBy
      :> QueryParam "offset" OffSet
      :> QueryParam "limit" Limit
      :> Get '[JSON] [News]
    :<|> "category"
      :> "create"
      :> BasicAuth "user" UserPublic
      :> QueryParam "root" Category
      :> QueryParam' '[Strict] "category_name" Category
      :> Get '[JSON] NewsCategory
    :<|> "category" :> "get_tree" :> Get '[JSON] NewsCategory
    :<|> "category"
      :> "change"
      :> BasicAuth "user" UserPublic
      :> QueryParam' '[Strict] "category_name" Category
      :> QueryParam "new_root" Category
      :> QueryParam "new_name" Category
      :> Get '[JSON] NewsCategory
    :<|> "create_news"
      :> "new"
      :> BasicAuth "user" UserPublic
      :> ReqBody '[JSON] NewsCreate
      :> Post '[JSON] News
    :<|> "create_news"
      :> "edit"
      :> BasicAuth "user" UserPublic
      :> QueryParam "news_uuid" UUID
      :> QueryParam "text" Content
      :> QueryParam "news_new_name" NameNews
      :> QueryParam "category" Category
      :> QueryParam "public" FlagPublished
      :> QueryParams "photos" Photo
      :> QueryParams "new_photos" Base64
      :> Get '[JSON] News
    :<|> "user"
      :> "create"
      :> BasicAuth "user" UserPublic
      :> QueryParam "name" Name
      :> QueryParam "login" Login
      :> QueryParam "password" Password
      :> QueryParam "make_news" FlagMakeNews
      :> QueryParam "admin" FlagAdmin
      :> Get '[JSON] UserPublic
    :<|> "user"
      :> "list"
      :> QueryParam "offset" OffSet
      :> QueryParam "limit" Limit
      :> Get '[JSON] [UserPublic]
    :<|> "photo" :> "get" :> QueryParam "name_photo" Photo :> Get '[JSON] Base64 -- ByteString

api :: Proxy API
api = Proxy

type GetNewsPublic m =
  Maybe DayAt ->
  Maybe DayUntil ->
  Maybe DaySince ->
  Maybe Name ->
  Maybe Category ->
  Maybe UUID ->
  Maybe NewsName ->
  Maybe Content ->
  Maybe ForString ->
  Maybe SortBy ->
  Maybe OffSet ->
  Maybe Limit ->
  m [News]

type GetNewsPrivate m =
  Maybe DayAt ->
  Maybe DayUntil ->
  Maybe DaySince ->
  Maybe Category ->
  Maybe UUID ->
  Maybe NewsName ->
  Maybe Content ->
  Maybe ForString ->
  Maybe FlagPublished ->
  Maybe SortBy ->
  Maybe OffSet ->
  Maybe Limit ->
  m [News]

type CategoryCreate m = Maybe Category -> Maybe Category -> m NewsCategory

type CategoryGetTree m = m NewsCategory

type CategoryChange m = Maybe Category -> Maybe Category -> Maybe Category -> m NewsCategory

type CreateNewsNew m = NewsCreate -> m News

type CreateNewsEdit m =
  Maybe UUID ->
  Maybe Content ->
  Maybe NameNews ->
  Maybe Content ->
  Maybe FlagPublished ->
  [Photo] ->
  [Base64] ->
  m News

type UserCreate m =
  Maybe Name ->
  Maybe Login ->
  Maybe Password ->
  Maybe FlagMakeNews ->
  Maybe FlagAdmin ->
  m UserPublic

type UserList m = Maybe OffSet -> Maybe Limit -> m [UserPublic]

type PhotoGet m = Maybe Photo -> m Base64

getNewsPublic ::
  GetNewsPublic ClientM
getNewsPrivate ::
  BasicAuthData ->
  GetNewsPrivate ClientM
categoryCreate :: BasicAuthData -> CategoryCreate ClientM
categoryGetTree :: CategoryGetTree ClientM
categoryChange :: BasicAuthData -> CategoryChange ClientM
createNewsNew :: BasicAuthData -> CreateNewsNew ClientM
createNewsEdit ::
  BasicAuthData ->
  CreateNewsEdit ClientM
userCreate ::
  BasicAuthData ->
  UserCreate ClientM
userList :: UserList ClientM
photoGet :: PhotoGet ClientM
getNewsPublic :<|> getNewsPrivate :<|> categoryCreate :<|> categoryGetTree :<|> categoryChange :<|> createNewsNew :<|> createNewsEdit :<|> userCreate :<|> userList :<|> photoGet = client api
