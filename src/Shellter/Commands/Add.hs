{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Shellter.Commands.Add
  ( run,
  )
where

import Control.Applicative
import Data.List
import Data.Maybe
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

saveEntry :: String -> String -> IO ()
saveEntry cmd' path =
  liftA2
    ( \currentTime ->
        addEntryIfDoesNotExist
          HistoryEntry
            { projectPath = path,
              lastUsed = formatDate currentTime,
              hits = 0,
              cmd = cmd'
            }
          . map
            ( \t ->
                -- TODO: add Eq instance to HistoryEntry
                if projectPath t == path && cmd t == cmd'
                  then t {hits = hits t + 1, lastUsed = formatDate currentTime}
                  else t
            )
    )
    getCurrentTime
    readHistoryFile
    >>= writeHistoryFile

run :: String -> String -> IO ()
run path cmd = findProjectRoot path >>= saveEntry cmd . fromMaybe ""
