module Control.Server.News where

import Prelude as P

import Control.Applicative

import Data.Text
import Data.Vector as V
import Data.Time.Calendar.OrdinalDate

import qualified Control.Server.Photo as Photo

import Data.News
import Data.Types

data Handle m = Handle
  { handlePhoto :: Photo.Handle
  , hGetNewsDay :: DayAt -> DayUntil -> m [News]
  , hPutNews :: News -> m () -- Bool
  , hGetNews :: Name -> NewsName -> m (Maybe News)
  , hModifNews :: Name -> NewsName -> (News -> News) -> m () -- Bool 
  , hGetDay :: m Day
  }

handleCreateNews :: Monad m => Handle m -> Login -> Name -> NewsCreate -> m News
handleCreateNews h l name nc = do
  let vbs = newPhotoNewsCreate nc
  vnpic <- mapM (\bs-> Photo.hPutPhoto (handlePhoto h) ) vbs
  d <- hGetDay h
  let news = n d ((photoNewCreate nc) V.++ vnpic)
  hPutNews h news
  return $ news
  where
    n d vp = News 
      { nameNews = nameNewsCreate nc
      , loginAothor = l
      , nameAothor = name
      , dateCreationNews = d
      , categoryNews = categoryNewsCreate nc
      , textNews = textNewsCreate nc
      , photoNews = vp
      , publicNews = publicNewsCreate
      }

-- handleGetNewsDay :: Handle
handleFind :: Monad m
           => Handle m
           -> Search
           -> m [News]
handleFind h 
    (Search mDayAt mDayUntil mDaySince mAothor mCategory mNewsNam mContent mForString mFlagPublished mSortBy mOffSet mLimit) = do -- error "Not implement"
  lNews <- hGetNewsDay h d1 d2
  return $ offSetLimitNews mOffSet mLimit $ sortNews mSortBy $ filters lNews
  where
    (d1,d2) = maybe (error "no defult date set") id $ ( do
      d1' <- mDayAt
      d2' <- mDayUntil
      return (d1',d2')
      ) <|> ( do
      d' <- mDaySince
      return (d',d')
      )
    filters = filterName mAothor . 
      filterCategory mCategory . 
      filterNewsName mNewsNam .
      filterContent mContent .
      filterForString mForString .
      filterFlagPublished mFlagPublished

offSetLimitNews :: Maybe OffSet -> Maybe Limit -> [News] -> [News]
offSetLimitNews = error "Not implement"

sortNews :: Maybe SortBy -> [News] -> [News]
sortNews = error "Not implement"

filterName :: Maybe Name -> [News] -> [News]
filterName (Just author) = P.filter undefined

filterCategory :: Maybe Category -> [News] -> [News]
filterCategory = undefined

filterNewsName :: Maybe NewsName -> [News] -> [News]
filterNewsName = error "Not implement"

filterContent :: Maybe Content -> [News] -> [News]
filterContent = error "Not implement"

filterForString :: Maybe ForString -> [News] -> [News]
filterForString = error "Not implement"

filterFlagPublished :: Maybe FlagPublished -> [News] -> [News]  
filterFlagPublished = error "Not implement"
