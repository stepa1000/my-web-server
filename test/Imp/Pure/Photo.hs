module Imp.Pure.Photo (purePhoto, StatePhoto) where

import Control.Monad
import Control.Monad.State.Lazy as ST
import Control.Server.Photo as SPhoto
import Data.Map
import Data.Maybe
import Data.Types
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
  stdgen <- gets snd
  let (uuidPhoto, stdgen2) = random stdgen
  modify $ \(mapPhoto, _) -> (insert uuidPhoto base64 mapPhoto, stdgen2)
  return uuidPhoto

pureGetPhoto :: Photo -> StatePhoto (Maybe Base64)
pureGetPhoto photo = do
  state $ \(mapPhoto, stdgen) -> (Data.Map.lookup photo mapPhoto, (mapPhoto, stdgen))
