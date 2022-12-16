module Control.Server.News where

import Prelude as P

import Control.Applicative

import Data.Text
import Data.ByteString
import Data.Vector as V
import Data.Time.Calendar.OrdinalDate

import qualified Control.Server.Photo as Photo

import Data.News
import Data.Types

data Handle m = Handle
  { handlePhoto :: Photo.Handle m
  , hGetNewsDay :: DayAt -> DayUntil -> m [News]
  , hPutNews :: News -> m () -- Bool
  , hGetNews :: Name -> NewsName -> m (Maybe News)
  , hModifNews :: Name -> NewsName -> (News -> News) -> m () -- Bool 
  , hGetDay :: m Day
  }

handleEditNews :: Monad m 
               => Handle m
               -> Name 
               -> NameNews -- old   
               -> Maybe Content
               -> Maybe NameNews -- new
               -> Maybe Category
               -> Maybe FlagPublished
               -> Vector Photo
               -> Vector Base64 -- ByteString
               -> m (Maybe News)
handleEditNews h author nameN content newNameNews category flagP vP vB64 = do
  vnpic <- P.mapM (Photo.hPutPhoto (handlePhoto h) ) vB64
  mn <- hGetNews h author nameN
  case mn of
    (Just n) -> do
      let n2 = (editNews n) {photoNews = vP V.++ vnpic}
      hModifNews h author nameN (const n2)
      return $ Just n2
    _ -> return Nothing
  where
    editNews = editNewsContent content . 
      editNewsNameNews newNameNews . 
      editNewsCategory category . 
      editFlagPublished flagP
--    newContent = maybe

editNewsContent :: Maybe Content -> News -> News
editNewsContent (Just c) n = n {categoryNews = c}
editNewsContent Nothing n = n

editNewsNameNews :: Maybe NameNews -> News -> News
editNewsNameNews (Just nn) n = n {nameNews = nn}
editNewsNameNEws Nothing n = n

editNewsCategory :: Maybe Category -> News -> News
editNewsCategory (Just c) n = n {categoryNews = c}
editNewsCategory Nothing n = n

editFlagPublished :: Maybe FlagPublished -> News -> News
editFlagPublished (Just f) n = n {publicNews = f}
editFlagPublished Nothing n = n

handleCreateNews :: Monad m => Handle m {- > Vector ByteString -} -> Login -> Name -> NewsCreate -> m News
handleCreateNews h {- vbs -} l name nc = do
  let vbs = newPhotoNewsCreate nc
  vnpic <- P.mapM (Photo.hPutPhoto (handlePhoto h) ) vbs
  d <- hGetDay h
  let news = n d ((photoNewsCreate nc) V.++ vnpic)
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
      , publicNews = publicNewsCreate nc
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
