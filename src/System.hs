module System where

import Teleport (Portals)
import qualified Turtle as T
import qualified Data.Map.Lazy as M
import Data.Maybe (fromMaybe)
import Data.Aeson (encodeFile, decodeFileStrict)

getPwd :: IO String
getPwd = fmap T.encodeString T.pwd

getFolder :: IO String
getFolder = do
  home <- T.home
  let folder = T.encodeString home ++ "/.teleport"
  exists <- (T.testdir . T.decodeString) folder
  if exists
     then return folder
     else (T.mkdir . T.decodeString) folder >> return folder

getFile :: IO String
getFile = do
  folder <- getFolder
  let file = folder ++ "/portals.json"
  exists <- (T.testfile . T.decodeString) file
  if exists
     then return file
     else (T.touch . T.decodeString) file >> return file

getPortals :: IO Portals
getPortals = getFile >>= (\file ->
  fmap (fromMaybe M.empty) (decodeFileStrict file))
