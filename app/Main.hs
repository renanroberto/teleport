{-# LANGUAGE OverloadedStrings #-}

module Main where

import Teleport
import qualified Turtle as T
import Data.Map.Lazy (empty)
import Data.Maybe (fromMaybe)
import Data.Aeson (encodeFile, decodeFileStrict)


main :: IO ()
main = do
  pwd <- getPwd
  folder <- getFolder -- not used in main
  file <- getFile folder
  portals <- getPortals file

  args <- T.arguments
  case args of
    ["list"] -> (putStr . listPortals) portals
    ["add", label] -> encodeFile file $ addPortal (show label) pwd portals
    ["rm", label] -> encodeFile file $ removePortal (show label) portals
    ["go", label] -> case goToPortal (show label) portals of
                       Nothing -> putStrLn "Portal not found"
                       Just path -> putStr path >> T.exit (T.ExitFailure 2)
    _ -> putStrLn "Invalid command"


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

getFile :: FilePath -> IO String
getFile folder = do
  let file = folder ++ "/portals.json"
  exists <- (T.testfile . T.decodeString) file
  if exists
     then return file
     else (T.touch . T.decodeString) file >> return file

getPortals :: FilePath -> IO Portals
getPortals file = fmap (fromMaybe empty) (decodeFileStrict file)

