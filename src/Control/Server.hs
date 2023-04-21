{-# LANGUAGE OverloadedStrings #-}

module Control.Server
  ( Handle (..),
    handleServerFind,
    handleCategoryCreate,
    handleCategoryGet,
    handleCategoryChange,
    handleCreateNewsNew,
    handleServerEditNews,
    handleUserCreate,
    handleUserList,
    handlePhotoGet,
  )
where

-- import Servant.Server as Servant

import qualified Control.Logger as Logger
import qualified Control.Server.Authorization as ServerAuthorization
import qualified Control.Server.Category as ServerCategory
import qualified Control.Server.News as ServerNews
import qualified Control.Server.Photo as ServerPhoto
import Data.News
import Data.Types
import Data.UUID
import Data.User
import Data.Vector

-- The handler that connects the subhandlers
-- to provide to the user requests through the API.
data Handle m = Handle
  { -- | The handler for the logging.
    handleLogger :: Logger.Handle m,
    -- | To access the database to work with the news.
    handleNews :: ServerNews.Handle m,
    -- | To access the database to work with the category.
    handleCategory :: ServerCategory.Handle m,
    -- | To access the database to work with the user.
    handleAuthorization :: ServerAuthorization.Handle m
  }

-- Find news.
--
-- It accesses the database through a handler that lies in the structure
-- being transferred. It sets the default value if there is no data on the user.
--
-- The function serves as an intermediary between the API and the search services.
handleServerFind ::
  Handle m ->
  Maybe UserPublic ->
  Search ->
  m [News]
handleServerFind
  h
  Nothing
  s =
    do
      ServerNews.handleFind (handleNews h) Nothing $
        s {mFlagPublished = Just True}
handleServerFind
  h
  muserPublic
  s = ServerNews.handleFind (handleNews h) (loginUser <$> muserPublic) s

handleCategoryCreate ::
  Monad m =>
  Handle m ->
  UserPublic ->
  Category -> -- is root
  Category ->
  m NewsCategory
handleCategoryCreate h userpublic mc c = do
  case userpublic of
    (UserPublic _ _ _ True _) -> do
      ServerCategory.hCreateCategory (handleCategory h) mc c
      ServerCategory.hGetCategory (handleCategory h)
    _ -> do
      ServerAuthorization.hAdminCheckFail (handleAuthorization h)
      ServerCategory.hGetCategory (handleCategory h)

handleCategoryGet :: Handle m -> m NewsCategory
handleCategoryGet h = ServerCategory.hGetCategory (handleCategory h)

handleCategoryChange ::
  Monad m =>
  Handle m ->
  UserPublic ->
  Category ->
  Maybe Category ->
  Maybe Category ->
  m NewsCategory
handleCategoryChange h userpublic cname croot cnewname = do
  case userpublic of
    (UserPublic _ _ _ True _) -> do
      ServerCategory.hChangeCategory (handleCategory h) cname croot cnewname
      ServerCategory.hGetCategory (handleCategory h)
    _ -> do
      ServerAuthorization.hAdminCheckFail (handleAuthorization h)
      ServerCategory.hGetCategory (handleCategory h)

handleCreateNewsNew :: Monad m => Handle m -> UserPublic -> NewsCreate -> m (Maybe News)
handleCreateNewsNew h userpublic nc = do
  case userpublic of
    (UserPublic name login _ _ True) -> do
      Just <$> ServerNews.handleCreateNews (handleNews h) login name nc
    _ -> do
      ServerAuthorization.hCreatorNewsCheckFail (handleAuthorization h)
      return Nothing

handleServerEditNews ::
  Monad m =>
  Handle m ->
  UserPublic ->
  UUID -> -- old
  Maybe Content ->
  Maybe NameNews -> -- new
  Maybe Category ->
  Maybe FlagPublished ->
  Vector Photo ->
  Vector Base64 ->
  m (Maybe News)
handleServerEditNews h userpublic nUUID mContent' mNameNews mCategory' mFlagP vPh vB64 = do
  case userpublic of
    (UserPublic _ lu _ _ True) -> do
      ServerNews.handleEditNews (handleNews h) lu nUUID mContent' mNameNews mCategory' mFlagP vPh vB64
    _ -> do
      ServerAuthorization.hCreatorNewsCheckFail (handleAuthorization h)
      return Nothing

handleUserCreate ::
  Monad m =>
  Handle m ->
  UserPublic ->
  Name ->
  Login ->
  Password ->
  FlagMakeNews ->
  FlagAdmin ->
  m (Maybe UserPublic)
handleUserCreate h userpublic name login password fMakeNews fAdmin = do
  case userpublic of
    (UserPublic _ _ _ True _) -> do
      u <- ServerAuthorization.hCreateUser (handleAuthorization h) name login password fMakeNews fAdmin
      return $ Just u
    _ -> do
      ServerAuthorization.hAdminCheckFail (handleAuthorization h)
      return Nothing

handleUserList :: Monad m => Handle m -> OffSet -> Limit -> m [UserPublic]
handleUserList h offset limit = do
  Logger.logInfo (handleLogger h) "call handleUserList"
  ServerAuthorization.hUserList (handleAuthorization h) offset limit

handlePhotoGet :: Handle m -> Photo -> m (Maybe Base64)
handlePhotoGet h =
  ServerPhoto.hGetPhoto (ServerNews.handlePhoto $ handleNews h)
