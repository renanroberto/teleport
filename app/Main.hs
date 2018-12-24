{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main where

import Teleport
import System (getPwd, getFolder, getFile, getPortals)

import Options.Applicative
import qualified Turtle as T
import Data.Aeson (encodeFile, decodeFileStrict)


data Command
  = CommandList
  | CommandAdd { label :: String }
  | CommandRemove { label :: String }
  | CommandGo { label :: String }


parseListCommand :: Parser Command
parseListCommand = pure CommandList

parseAddCommand :: Parser Command
parseAddCommand = CommandAdd <$> labelParser

parseRemoveCommand :: Parser Command
parseRemoveCommand = CommandRemove <$> labelParser

parseGoCommand :: Parser Command
parseGoCommand = CommandGo <$> labelParser

labelParser :: Parser String
labelParser = argument str (metavar "Label" <> help "The portal label")


parseCommand :: Parser Command
parseCommand = subparser $
  ( command
      "list"
      ( info
          (helper <*> parseListCommand)
          (fullDesc <> progDesc "List all portals")
      )
  )
  <>
  ( command
    "add"
    ( info
        (helper <*> parseAddCommand)
        (fullDesc <> progDesc "Add a new portal")
    )
  )
  <>
  ( command
    "rm"
    ( info
        (helper <*> parseRemoveCommand)
        (fullDesc <> progDesc "Remove the portal")
    )
  )
  <>
  ( command
    "go"
    ( info
        (helper <*> parseListCommand)
        (fullDesc <> progDesc "Go through the portal")
    )
  )


runList :: IO ()
runList = getPortals >>= (putStr . listPortals)

runAdd :: String -> IO ()
runAdd label = do
  pwd <- getPwd
  file <- getFile
  portals <- getPortals
  encodeFile file $ addPortal label pwd portals 

runRemove :: String -> IO ()
runRemove label = do
  file <- getFile
  portals <- getPortals
  encodeFile file $ removePortal label portals 

runGo :: String -> IO ()
runGo label = do
  portals <- getPortals
  case goToPortal label portals of
    Nothing -> putStrLn "Portal not found"
    Just path -> putStr path >> T.exit (T.ExitFailure 2)


main :: IO ()
main = do
  command <- execParser $ info
    (helper <*> parseCommand)
    ( fullDesc
   <> progDesc "A teleporter between folders"
   <> header "Teleporter"
    )
  run command

run :: Command -> IO ()
run CommandList = runList
run CommandAdd{..} = runAdd label
run CommandRemove{..} = runRemove label
run CommandGo{..} = runGo label

