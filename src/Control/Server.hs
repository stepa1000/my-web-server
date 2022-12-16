module Control.Server where

-- import Servant.Server as Servant

import qualified Control.Server.News as ServerNews
import qualified Control.Server.Category as ServerCategory
import qualified Control.Server.Authorization as ServerAuthorization
import qualified Control.Logger as Logger

import qualified Control.Server.Photo as ServerPhoto

import Data.Maybe
import Data.Vector

import Data.News
import Data.User
import Data.Types

data Handle m = Handle
  { handleLogger :: Logger.Handle m
  , handleNews :: ServerNews.Handle m
  , handleCategory :: ServerCategory.Handle m
  , handleAuthorization :: ServerAuthorization.Handle m
  -- , hHoistToServerHandle :: forall x. m x -> Servant.Handler x
  }

handleServerFind :: Monad m 
           => Handle m
           -> Maybe Logined
           -> Search
           -> m [News]
handleServerFind h Nothing 
    (Search mDayAt mDayUntil mDaySince mAothor mCategory mNewsNam mContent mForString mFlagPublished mSortBy mOffSet mLimit) = do
  ServerNews.handleFind (handleNews h) $ 
    Search mDayAt mDayUntil mDaySince mAothor mCategory mNewsNam mContent mForString (Just True) mSortBy mOffSet mLimit
handleServerFind h (Just logined) 
    (Search mDayAt mDayUntil mDaySince mAothor mCategory mNewsNam mContent mForString mFlagPublished mSortBy mOffSet mLimit) = do
  b <- ServerAuthorization.handleCheckAccount 
    (handleAuthorization h) logined
  if (isJust b)
  then do
    ServerNews.handleFind (handleNews h) $
      Search mDayAt mDayUntil mDaySince mAothor mCategory mNewsNam mContent mForString mFlagPublished mSortBy mOffSet mLimit
  else do
    ServerNews.handleFind (handleNews h) $
      Search mDayAt mDayUntil mDaySince mAothor mCategory mNewsNam mContent mForString (Just True) mSortBy mOffSet mLimit

handleCategoryCreate :: Monad m
                     => Handle m
                     -> Logined
                     -> Maybe Category -- root
                     -> Category
                     -> m NewsCategory
handleCategoryCreate h logined mc c = do -- error "Not implement"
  userpublic <- ServerAuthorization.handleCheckAccount 
    (handleAuthorization h) logined
  case userpublic of
    (Just (UserPublic _ _ _ True _)) -> do
      ServerCategory.hCreateCategory (handleCategory h) mc c
      ServerCategory.hGetCategory (handleCategory h)
    _ -> ServerCategory.hGetCategory (handleCategory h)

handleCategoryGet :: Monad m => Handle m -> m NewsCategory
handleCategoryGet h = ServerCategory.hGetCategory (handleCategory h)

handleCategoryChange :: Monad m 
                     => Handle m 
                     -> Logined
                     -> Category 
                     -> Maybe Category 
                     -> Maybe Category
                     -> m NewsCategory
handleCategoryChange h logined cname croot cnewname = do
  userpublic <- ServerAuthorization.handleCheckAccount 
    (handleAuthorization h) logined
  case userpublic of
    (Just (UserPublic _ _ _ True _)) -> do
      ServerCategory.hChangeCategory (handleCategory h) cname croot cnewname
      ServerCategory.hGetCategory (handleCategory h)
    _ -> ServerCategory.hGetCategory (handleCategory h)

handleCreateNewsNew :: Monad m => Handle m -> Logined -> NewsCreate -> m (Maybe News)
handleCreateNewsNew h logined nc = do
  userpublic <- ServerAuthorization.handleCheckAccountStrong 
    (handleAuthorization h) logined
  case userpublic of
    (Just (UserPublic name login _ _ True)) -> do
      fmap Just $ ServerNews.handleCreateNews (handleNews h) login name nc
    _ -> do
      ServerAuthorization.hCreatorNewsCheckFail
      return Nothing

handleServerEditNews :: Monad m 
                     => Handle m 
                     -> Logined 
                     -> NameNews -- old   
                     -> Maybe Content
                     -> Maybe NameNews -- new
                     -> Maybe Category
                     -> Maybe FlagPublished
                     -> Vector Photo
                     -> Vector Base64 -- ByteString
                     -> m (Maybe News)
handleServerEditNews h logined nameN mContent mNameNews mCategory mFlagP vPh vB64 = do
  userpublic <- ServerAuthorization.handleCheckAccount 
    (handleAuthorization h) logined
  case userpublic of
    (Just (UserPublic name _ _ _ True)) -> do
      ServerNews.handleEditNews (handleNews h) name nameN mContent mNameNews mCategory mFlagP vPh vB64
  
handleUserCreate :: Monad m
                 => Handle m -> Logined -> Name -> Login -> Password -> FlagMakeNews -> FlagAdmin -> m (Maybe UserPublic)
handleUserCreate h logined name login password fMakeNews fAdmin = do
  userpublic <- ServerAuthorization.handleCheckAccount 
    (handleAuthorization h) logined
  case userpublic of
    (Just (UserPublic _ _ _ True _)) -> do
      u <- ServerAuthorization.hCreateUser (handleAuthorization h) name login password fMakeNews fAdmin
      return $ Just u
    _ -> return Nothing

handleUserList :: Monad m => Handle m -> m [UserPublic]
handleUserList h = do
  ServerAuthorization.hUserList (handleAuthorization h)

handlePhotoGet :: Monad m => Handle m -> Photo -> m (Maybe Base64)
handlePhotoGet h p = do
  ServerPhoto.hGetPhoto (ServerNews.handlePhoto $ handleNews h) p

{-
data Event 
  = QueryNews
      (Maybe Logined)
      (Maybe DayAt)
      (Maybe DayUntil)
      (Maybe DaySince)
      (Maybe Name)
      (Maybe Category)
      (Maybe NewsName)
      (Maybe Content)
      (Maybe ForString)
      (Maybe FlagPublished)
      (Maybe SortBy)    
      (Maybe OffSet)
      (Maybe Limit)
  | CategotyCreat (Maybe Category) Category
  | CategoryGetTree
  | CategoryChange Logined Category (Maybe Category) (Maybe Category)
-}
