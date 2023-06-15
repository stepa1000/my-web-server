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
  hServer
  Nothing
  search =
    do
      ServerNews.handleFind (handleNews hServer) Nothing $
        search {mFlagPublished = Just True}
handleServerFind
  hServer
  muserPublic
  search = ServerNews.handleFind (handleNews hServer) (loginUser <$> muserPublic) search

handleCategoryCreate ::
  Monad m =>
  Handle m ->
  UserPublic ->
  Category -> -- is root
  Category ->
  m NewsCategory
handleCategoryCreate hServer userpublic categoryRoot category = do
  case userpublic of
    (UserPublic _ _ _ True _) -> do
      ServerCategory.hCreateCategory (handleCategory hServer) categoryRoot category
      ServerCategory.hGetCategory (handleCategory hServer)
    _ -> do
      ServerAuthorization.hAdminCheckFail (handleAuthorization hServer)
      ServerCategory.hGetCategory (handleCategory hServer)

handleCategoryGet :: Handle m -> m NewsCategory
handleCategoryGet hServer = ServerCategory.hGetCategory (handleCategory hServer)

handleCategoryChange ::
  Monad m =>
  Handle m ->
  UserPublic ->
  Category ->
  Maybe Category ->
  Maybe Category ->
  m NewsCategory
handleCategoryChange hServer userpublic categoryName categoryRoot categoryNewName = do
  case userpublic of
    (UserPublic _ _ _ True _) -> do
      ServerCategory.hChangeCategory (handleCategory hServer) categoryName categoryRoot categoryNewName
      ServerCategory.hGetCategory (handleCategory hServer)
    _ -> do
      ServerAuthorization.hAdminCheckFail (handleAuthorization hServer)
      ServerCategory.hGetCategory (handleCategory hServer)

handleCreateNewsNew :: Monad m => Handle m -> UserPublic -> NewsCreate -> m (Maybe News)
handleCreateNewsNew hServer userpublic newsCreate = do
  case userpublic of
    (UserPublic name login _ _ True) -> do
      Just <$> ServerNews.handleCreateNews (handleNews hServer) login name newsCreate
    _ -> do
      ServerAuthorization.hCreatorNewsCheckFail (handleAuthorization hServer)
      return Nothing

handleServerEditNews ::
  Monad m =>
  Handle m ->
  UserPublic ->
  UUID ->
  Maybe Content ->
  Maybe NameNews -> -- new
  Maybe Category ->
  Maybe FlagPublished ->
  Vector Photo ->
  Vector Base64 ->
  m (Maybe News)
handleServerEditNews hServer userpublic nUUID mContent' mNameNews mCategory' mFlagPub vPhoto vBase64 = do
  case userpublic of
    (UserPublic _ login _ _ True) -> do
      ServerNews.handleEditNews (handleNews hServer) login nUUID mContent' mNameNews mCategory' mFlagPub vPhoto vBase64
    _ -> do
      ServerAuthorization.hCreatorNewsCheckFail (handleAuthorization hServer)
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
handleUserCreate hServer userpublic name login password flagMakeNews flagAdmin = do
  case userpublic of
    (UserPublic _ _ _ True _) -> do
      vUser <- ServerAuthorization.handleCreateUserCheck (handleAuthorization hServer) name login password flagMakeNews flagAdmin
      return $ vUser
    _ -> do
      ServerAuthorization.hAdminCheckFail (handleAuthorization hServer)
      return Nothing

handleUserList :: Monad m => Handle m -> OffSet -> Limit -> m [UserPublic]
handleUserList hServer offset limit = do
  Logger.logInfo (handleLogger hServer) "call handleUserList"
  ServerAuthorization.hUserList (handleAuthorization hServer) offset limit

handlePhotoGet :: Handle m -> Photo -> m (Maybe Base64)
handlePhotoGet hServer =
  ServerPhoto.hGetPhoto (ServerNews.handlePhoto $ handleNews hServer)
