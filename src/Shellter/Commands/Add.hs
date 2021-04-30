{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Shellter.Commands.Add
  ( run,
  )
where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.String.Utils
import Data.Time
import Shellter.HistoryFile
import System.Directory

hasGitFolder :: FilePath -> IO Bool
hasGitFolder path = doesPathExist $ path ++ "/.git"

findProjectRoot :: String -> IO (Maybe String)
findProjectRoot "/" = return Nothing
findProjectRoot currPath =
  hasGitFolder currPath
    >>= ( \case
            True -> return (Just currPath)
            False -> canonicalizePath (currPath ++ "/../") >>= findProjectRoot
        )

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

addEntryIfDoesNotExist :: HistoryEntry -> [HistoryEntry] -> [HistoryEntry]
addEntryIfDoesNotExist toAdd entries =
  (++) entries
    . ( \case
          Just a -> []
          Nothing -> [toAdd]
      )
    . find (\t -> projectPath toAdd == projectPath t && cmd toAdd == cmd t)
    $ entries

processList :: String -> String -> Int -> UTCTime -> [HistoryEntry] -> [HistoryEntry]
processList path cmd errorCode currentTime =
  addEntryIfDoesNotExist entryToAdd
    . map
      ( \entry ->
          if entryToAdd == entry
            then entry {hits = hits entry + 1, lastUsed = formatDate currentTime}
            else entry
      )
  where
    entryToAdd =
      HistoryEntry
        { projectPath = path,
          lastUsed = formatDate currentTime,
          hits = 1,
          cmd = cmd,
          lastErrorCode = errorCode
        }

validate :: String -> Bool
validate "" = False
validate _ = True

saveEntry :: String -> String -> Int -> IO ()
saveEntry path cmd errorCode =
  if validate cmd
    then
      processList path processedCmd errorCode
        <$> getCurrentTime
        <*> readHistoryFile
        >>= writeHistoryFile
    else pure ()
  where
    processedCmd = strip cmd

run :: String -> String -> Int -> IO ()
run path cmd errorCode = findProjectRoot path >>= (\t -> saveEntry t cmd errorCode) . fromMaybe ""
