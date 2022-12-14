module Control.Server where

-- import Servant.Server as Servant

import qualified Control.Server.News as ServerNews
import qualified Control.Server.Category as ServerCategory
import qualified Control.Server.Authorization as ServerAuthorization
import qualified Control.Logger as Logger

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
  b <- ServerAuthorization.hCheckAccount 
    (handleAuthorization h) 
    (loginLogined logined) 
    (passwordLogined logined )
  if b
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
handleCategoryCreate = error "Not implement"

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
