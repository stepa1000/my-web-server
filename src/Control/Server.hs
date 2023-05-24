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
import Data.Maybe
import Data.News
import Data.Types
import Data.User
import Data.Vector

data Handle m = Handle
  { handleLogger :: Logger.Handle m,
    handleNews :: ServerNews.Handle m,
    handleCategory :: ServerCategory.Handle m,
    handleAuthorization :: ServerAuthorization.Handle m
  }

handleServerFind ::
  Handle m ->
  Maybe UserPublic ->
  Search ->
  m [News]
handleServerFind
  h
  Nothing
  (Search mDayAt' mDayUntil' mDaySince' mAothor' mCategory' mNewsNam' mContent' mForString' _ mSortBy' mOffSet' mLimit') = do
    ServerNews.handleFind (handleNews h) $
      Search mDayAt' mDayUntil' mDaySince' mAothor' mCategory' mNewsNam' mContent' mForString' (Just True) mSortBy' mOffSet' mLimit'
handleServerFind
  h
  muserPublic
  (Search mDayAt' mDayUntil' mDaySince' mAothor' mCategory' mNewsNam' mContent' mForString' mFlagPublished' mSortBy' mOffSet' mLimit') = do
    if isJust muserPublic
      then do
        ServerNews.handleFind (handleNews h) $
          Search mDayAt' mDayUntil' mDaySince' mAothor' mCategory' mNewsNam' mContent' mForString' mFlagPublished' mSortBy' mOffSet' mLimit'
      else do
        ServerNews.handleFind (handleNews h) $
          Search mDayAt' mDayUntil' mDaySince' mAothor' mCategory' mNewsNam' mContent' mForString' (Just True) mSortBy' mOffSet' mLimit'

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
  NameNews -> -- old
  Maybe Content ->
  Maybe NameNews -> -- new
  Maybe Category ->
  Maybe FlagPublished ->
  Vector Photo ->
  Vector Base64 ->
  m (Maybe News)
handleServerEditNews h userpublic nameN mContent' mNameNews mCategory' mFlagP vPh vB64 = do
  case userpublic of
    (UserPublic _ lu _ _ True) -> do
      ServerNews.handleEditNews (handleNews h) lu nameN mContent' mNameNews mCategory' mFlagP vPh vB64
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
