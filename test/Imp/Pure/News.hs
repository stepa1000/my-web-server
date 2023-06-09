{-# LANGUAGE TypeApplications #-}

module Imp.Pure.News (StateNews, pureNews) where

import Control.Monad.State.Lazy
import Control.Server.News as SNews
import Control.Server.Photo
import Data.Bool
import Data.Eq
import Data.Foldable
import Data.Imp.Server.News (sortNews)
import Data.List.CommonSubstring
import Data.Map as Map
import Data.Maybe as Maybe
import Data.News
import Data.Ord
import Data.Text
import Data.Time.Calendar.OrdinalDate
import Data.Types
import Data.UUID
import Imp.Pure.Photo
import System.Random
import Prelude (Char, id, snd, ($), (.), (<$>))
import qualified Prelude as P

type StateNews = StateT (Map UUID News) StatePhoto

pureNews :: Day -> SNews.Handle StateNews
pureNews day =
  SNews.Handle
    { handlePhoto = purePhoto {hPutPhoto = lift . hPutPhoto purePhoto, hGetPhoto = lift . hGetPhoto purePhoto},
      hSearchNews = pureSearchNews,
      hPutNews = purePutNew,
      hGenUUID = pureGenUUID,
      hGetNews = pureGetNews,
      hModifNews = pureModifNews,
      hGetDay = pureGetDay day
    }

pureSearchNews :: Maybe Login -> Search -> StateNews [News]
pureSearchNews _ search = do
  mapNews <- get @(Map UUID News)
  return $ (\l -> P.take (maybe (P.length l) id $ mLimit search) l) $ P.drop (maybe 0 id $ mOffSet search) $ sortNews (mSortBy search) $ fmap snd $ Map.toList $ Map.filter filterNews mapNews
  where
    filterNews news =
      and
        [ maybe True ((==) (dateCreationNews news)) (mDayAtSearch search),
          maybe True ((<) (dateCreationNews news)) (mDayUntil search),
          maybe True ((>) (dateCreationNews news)) (mDaySince search),
          maybe True ((==) (nameNews news)) (mAuthor search),
          maybe True ((==) (categoryNews news)) (mCategory search),
          maybe True ((==) (uuidNews news)) (mNewsUUID search),
          maybe True ((==) (nameNews news)) (mNewsName search),
          maybe True ((==) (textNews news)) (mContent search),
          maybe True (not . P.null . longestSubstring @(Char) (unpack $ textNews news)) (unpack <$> mForString search),
          maybe True ((==) (publicNews news)) (mFlagPublished search)
        ]

--    ifUndefined str = const True str
--    ifUndefined _ = False

purePutNew :: News -> StateNews ()
purePutNew news = modify @(Map UUID News) $
  \mapNews -> insert (uuidNews news) news mapNews

pureGenUUID :: StateNews UUID
pureGenUUID = do
  stdGen <- lift $ gets @(Map Photo Base64, StdGen) snd
  let (uuid, stdGen2) = random stdGen
  lift $
    modify @(Map Photo Base64, StdGen)
      (\(mapPhoto, _) -> (mapPhoto, stdGen2))
  return uuid

pureGetNews :: UUID -> StateNews (Maybe News)
pureGetNews uuid =
  state @(Map UUID News) $ \mapNews -> (lookup uuid mapNews, mapNews)

pureModifNews :: UUID -> (News -> News) -> StateNews ()
pureModifNews uuid act =
  modify @(Map UUID News) $ \mapNews -> adjust act uuid mapNews

pureGetDay :: Day -> StateNews Day
pureGetDay day = return day
