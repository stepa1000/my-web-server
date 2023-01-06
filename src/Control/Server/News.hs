module Control.Server.News 
  ( Handle(..)
  , handleEditNews
  , handleCreateNews
  , handleFind
  ) where

import Prelude as P

-- import Control.Applicative

-- import Data.Text
-- import Data.ByteString
import Data.Vector as V
import Data.Time.Calendar.OrdinalDate

import qualified Control.Server.Photo as Photo

import Data.News
import Data.Types

data Handle m = Handle
  { handlePhoto :: Photo.Handle m
  , hSearchNews :: Search -> m [News]
  , hPutNews :: News -> m () -- Bool
  , hGetNews :: NewsName -> m (Maybe News)
  , hModifNews :: NewsName -> (News -> News) -> m () -- Bool 
  , hGetDay :: m Day
  }

handleEditNews :: Monad m 
               => Handle m
               -- -> Name 
               -> NameNews -- old   
               -> Maybe Content
               -> Maybe NameNews -- new
               -> Maybe Category
               -> Maybe FlagPublished
               -> Vector Photo
               -> Vector Base64 -- ByteString
               -> m (Maybe News)
handleEditNews h {-_ author-} nameN content newNameNews category flagP vP vB64 = do
  vnpic <- P.mapM (Photo.hPutPhoto (handlePhoto h) ) vB64
  mn <- hGetNews h nameN
  case mn of
    (Just n) -> do
      let n2 = (editNews n) {photoNews = vP V.++ vnpic}
      hModifNews h nameN (const n2)
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
editNewsNameNews _ n = n

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
      , loginAuthor = l
      , nameAuthor = name
      , dateCreationNews = d
      , categoryNews = categoryNewsCreate nc
      , textNews = textNewsCreate nc
      , photoNews = vp
      , publicNews = publicNewsCreate nc
      }

-- handleGetNewsDay :: Handle
handleFind :: Handle m
           -> Search
           -> m [News]
handleFind h search = 
--    (Search mDayAt mDayUntil mDaySince mAothor mCategory mNewsNam mContent mForString mFlagPublished mSortBy mOffSet mLimit) = do -- error "Not implement"
  hSearchNews h search

