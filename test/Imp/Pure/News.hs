module Imp.Pure.News where

import qualified Control.Logger as Logger
import Control.Monad.State.Lazy
import qualified Control.Server.News as SNews
import Data.ByteString
import Data.Foldable
import Data.Imp.Database
import qualified Data.Imp.Server.Photo as ImpSPhoto
import Data.List (sortBy)
import Data.Maybe as Maybe
import Data.News
import Data.String
import Data.Text.Internal
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Types
import Data.UUID
import Data.Utils
import Data.Vector as V
import Data.Yaml as Y
import Database.Beam as Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import Database.Beam.Query.Internal
import System.Random
import Prelude as P
import Imp.Pure.Photo
import Data.Map
import Data.List.CommonSubstring
import Data.Imp.Server.News (sortNews)

type StateNews = SatetT (Map UUID News) StatePhoto

pureNews :: Day -> Handle StateNews
pureNews day =
  Handle 
    { handlePhoto = purePhoto,
      hSearchNews = pureSearchNews,
      hPutNews = purePutNew,
      hGenUUID = pureGenUUID,
      hGetNews = pureGetNews,
      hModifNews = pureModifNews
      hGetDay = pureGetDay day
    }

pureSearchNews :: Maybe Login -> Search -> StateNews [News]
pureSearchNews maybeLogin search = do
  mapNews <- get @_ @(Map UUID News)
  return $ (\l-> take (maybe (length l) id $ mLimit search)) $ drop (maybe 0 id $ mOffSet search) $ sortNews (mSortBy search) $ toList $ filter filterNews mapNews
  where
    filterNews news = and
      [ maybe True ((==) (dateCreationNews news)) (mDayAtSearch search)
      , maybe True ((<) (dateCreationNews news)) (mDayUntil search)
      , maybe True ((>) (dateCreationNews news)) (mDaySince search)
      , maybe True ((==) (nameNews news)) (mAuthor search)
      , maybe True ((==) (categoryNews news)) (mCategory search)
      , maybe True ((==) (uuidNews news)) (mNewsUUID search)
      , maybe True ((==) (nameNews news)) (mNewsName search)
      , maybe True ((==) (textNews news)) (mContent search)
      , maybe True (ifUndefined . longestSubstring (textNews news)) (mForString search)
      , maybe True ((==) (publicNews news)) (mFlagPublished search)
      ]
    ifUndefined str = True
    ifUndefined _ = False

purePutNew :: News -> StateNews ()
purePutNew news = modify @(Map UUID News) $
  \ mapNews -> insert (uuidNews news) news mapNews

pureGenUUID :: StateNews UUID
pureGenUUID = do
  stdGen <- gets @(Map Photo Base64, StdGen) snd
  let (uuid, stdGen2) = random stdGen
  modify @(Map Photo Base64, StdGen) 
    (\(mapPhoto,_)->(mapPhoto, stdGen2))
  return uuid

pureGetNews :: UUID -> StateNews (Maybe News)
pureGetNews uuid = 
  state @(Map UUID News) $ \ mapNews -> (lookup uuid mapNews, mapNews)

pureModifNews :: UUID -> (News -> News) -> StateNews (Maybe News)
pureModifNews uuid act = 
  state @(Map UUID News) $ \ mapNews -> (adjust act uuid mapNews, mapNews)

pureGetDay :: Day -> StateNews Day
pureGetDay day = return day
