module Control.Server.News 
  ( Handle(..)
  , handleEditNews
  , handleCreateNews
  , handleFind
  ) where

import Prelude as P

import Control.Monad

import Data.Vector as V
import Data.Time.Calendar.OrdinalDate

import qualified Control.Server.Photo as Photo

import Data.News
import Data.Types

data Handle m = Handle
  { handlePhoto :: Photo.Handle m
  , hSearchNews :: Search -> m [News]
  -- | create new news
  , hPutNews :: News -> m ()
  , hGetNews :: NewsName -> m (Maybe News)
  , hModifNews :: NewsName -> (News -> News) -> m () 
  , hGetDay :: m Day
  }

-- | changes the value of the field to the specified
handleEditNews :: Monad m 
               => Handle m
               -> Login
               -> NameNews -- old   
               -> Maybe Content
               -> Maybe NameNews -- new
               -> Maybe Category
               -> Maybe FlagPublished
               -> Vector Photo
               -> Vector Base64 -- ByteString
               -> m (Maybe News)
handleEditNews h login nameN content newNameNews category flagP vP vB64 = do
  vnpic <- P.mapM (Photo.hPutPhoto (handlePhoto h) ) vB64
  mn <- (\m-> m >>= f ) <$> hGetNews h nameN
  case mn of
    (Just n) -> do
      let n2 = (editNews n) {photoNews = vP V.++ vnpic}
      hModifNews h nameN (const n2)
      return $ Just n2
    _ -> return Nothing
  where
    f n = guard ((loginAuthor n) == login) >> return n
    editNews = editNewsContent content . 
      editNewsNameNews newNameNews . 
      editNewsCategory category . 
      editFlagPublished flagP

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

handleCreateNews :: Monad m => Handle m -> Login -> Name -> NewsCreate -> m News
handleCreateNews h l name nc = do
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

handleFind :: Handle m
           -> Search
           -> m [News]
handleFind h search = 
  hSearchNews h search

