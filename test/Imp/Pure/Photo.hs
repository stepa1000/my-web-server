module Imp.Pure.Photo where

import qualified Control.Logger as Logger
import Control.Monad
import qualified Control.Server.Photo as SPhoto
import Data.Imp.Database
import Data.Maybe
import Data.Types
import Data.UUID
import Data.Utils
import Database.Beam
import Database.Beam.Postgres as Beam
import Database.Beam.Postgres.Conduit as BPC
import System.Random
import Prelude as P

type StatePhoto = State (Map Photo Base64, StdGen)

purePhoto :: Handle StatePhoto
purePhoto =
  Handle
    { hPutPhoto = purePutPhoto,
      hGetPhoto = pureGetPhoto
    }

purePutPhoto :: Base64 -> StatePhoto Photo
purePutPhoto base64 = do
  stdgen <- gets $ snd
  let (uuidPhoto, stdgen2) = random stdgen
  modify $ \(mapPhoto, _) -> (insert uuidPhoto base64 mapPhoto, stdgen2)
  return uuidPhoto

pureGetPhoto :: Photo -> StatePhoto (Maybe Base64)
pureGetPhoto photo = do
  state $ \(mapPhoto, stdgen) -> (lookup photo mapPhoto, (mapPhoto, stdgen))
