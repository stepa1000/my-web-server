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
handleEditNews h login nameUUID content newNameNews category flagP vP vB64 = do
  vnpic <- P.mapM (Photo.hPutPhoto (handlePhoto h)) vB64
  mn <- (>>= f) <$> hGetNews h nameUUID
  case mn of
    (Just n) -> do
      let n2 = (editNews n) {photoNews = vP V.++ vnpic}
      hModifNews h nameUUID (const n2)
      return $ Just n2
    _ -> return Nothing
  where
    f n = guard (loginAuthor n == login) >> return n
    editNews =
      editNewsContent content
        . editNewsNameNews newNameNews
        . editNewsCategory category
        . editFlagPublished flagP

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

-- | Creates and adds news to the database.
--
-- Combines different pre-processors for news,
-- but does not check if the author can create news.
handleCreateNews :: Monad m => Handle m -> Login -> Name -> NewsCreate -> m News
handleCreateNews h l name nc = do
  let vbs = newPhotoNewsCreate nc
  vnpic <- P.mapM (Photo.hPutPhoto (handlePhoto h)) vbs
  d <- hGetDay h
  nuuid <- hGenUUID h
  let news = n nuuid d (photoNewsCreate nc V.++ vnpic)
  hPutNews h news
  return news
  where
    n nuuid d vp =
      News
        { uuidNews = nuuid,
          nameNews = nameNewsCreate nc,
          loginAuthor = l,
          nameAuthor = name,
          dateCreationNews = d,
          categoryNews = categoryNewsCreate nc,
          textNews = textNewsCreate nc,
          photoNews = vp,
          publicNews = publicNewsCreate nc
        }

handleFind ::
  Handle m ->
  Maybe Login ->
  Search ->
  m [News]
handleFind = hSearchNews
