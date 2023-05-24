module Control.Server.News
  ( Handle (..),
    handleEditNews,
    handleCreateNews,
    handleFind,
  )
where

import Control.Monad
import qualified Control.Server.Photo as Photo
import Data.News
import Data.Time.Calendar.OrdinalDate
import Data.Types
import Data.UUID
import Data.Vector as V
import Prelude as P

-- | Handler news.
--
-- Contains functions for accessing the database,
-- a handler for processing photos and obtaining the current time.
data Handle m = Handle
  { -- | Structure with functions to access the database to write, get photos.
    handlePhoto :: Photo.Handle m,
    -- | To search for news by the data specified in Search
    -- and provide unpublished for the user who created it.
    hSearchNews :: Maybe Login -> Search -> m [News],
    -- | Create new news.
    hPutNews :: News -> m (),
    -- | generation UUID for news.
    hGenUUID :: m UUID,
    -- | Finds the news by the specified identifier.
    hGetNews :: UUID -> m (Maybe News),
    -- | Changes the news by the specified identifier.
    hModifNews :: UUID -> (News -> News) -> m (),
    -- | Obtaining the current time.
    hGetDay :: m Day
  }

-- | changes the value of the field to the specified
handleEditNews ::
  Monad m =>
  Handle m ->
  Login ->
  UUID ->
  Maybe Content ->
  Maybe NameNews -> -- new
  Maybe Category ->
  Maybe FlagPublished ->
  Vector Photo ->
  Vector Base64 ->
  m (Maybe News)
handleEditNews hNews login nameUUID content newNameNews category flagPub vPhoto vBase64 = do
  vNamePic <- P.mapM (Photo.hPutPhoto (handlePhoto hNews)) vBase64
  mNews <- (>>= f) <$> hGetNews hNews nameUUID
  case mNews of
    (Just news) -> do
      let news2 = (editNews news) {photoNews = vPhoto V.++ vNamePic}
      hModifNews hNews nameUUID (const news2)
      return $ Just news2
    _ -> return Nothing
  where
    f n = guard (loginAuthor n == login) >> return n
    editNews =
      editNewsContent content
        . editNewsNameNews newNameNews
        . editNewsCategory category
        . editFlagPublished flagPub

editNewsContent :: Maybe Content -> News -> News
editNewsContent (Just content) news = news {categoryNews = content}
editNewsContent Nothing news = news

editNewsNameNews :: Maybe NameNews -> News -> News
editNewsNameNews (Just nameNews') news = news {nameNews = nameNews'}
editNewsNameNews _ news = news

editNewsCategory :: Maybe Category -> News -> News
editNewsCategory (Just category) news = news {categoryNews = category}
editNewsCategory Nothing news = news

editFlagPublished :: Maybe FlagPublished -> News -> News
editFlagPublished (Just flagPub) news = news {publicNews = flagPub}
editFlagPublished Nothing news = news

-- | Creates and adds news to the database.
--
-- Combines different pre-processors for news,
-- but does not check if the author can create news.
handleCreateNews :: Monad m => Handle m -> Login -> Name -> NewsCreate -> m News
handleCreateNews hNews login name newsCreate = do
  let vBase64 = newPhotoNewsCreate newsCreate
  vNamePic <- P.mapM (Photo.hPutPhoto (handlePhoto hNews)) vBase64
  day <- hGetDay hNews
  nuuid <- hGenUUID hNews
  let news = makeNews' nuuid day (photoNewsCreate newsCreate V.++ vNamePic)
  hPutNews hNews news
  return news
  where
    makeNews' nuuid day vpic =
      News
        { uuidNews = nuuid,
          nameNews = nameNewsCreate newsCreate,
          loginAuthor = login,
          nameAuthor = name,
          dateCreationNews = day,
          categoryNews = categoryNewsCreate newsCreate,
          textNews = textNewsCreate newsCreate,
          photoNews = vpic,
          publicNews = publicNewsCreate newsCreate
        }

handleFind ::
  Handle m ->
  Maybe Login ->
  Search ->
  m [News]
handleFind = hSearchNews
