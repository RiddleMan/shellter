{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Shellter.Commands.Add
  ( run,
  )
where

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
          Just a -> [a]
          Nothing -> []
      )
    . find (\t -> projectPath toAdd == projectPath t)
    $ entries

saveEntry :: String -> String -> IO ()
saveEntry cmd path =
  readHistoryFile
    >>= writeHistoryFile
      -- . addEntryIfDoesNotExist (HistoryEntry { projectPath = path, cmd = cmd, hits = 0, lastUsed =
      . map
        ( \t ->
            if projectPath t == path
              then t
              else t {hits = hits t + 1}
        )

run :: String -> String -> IO ()
run path cmd = findProjectRoot path >>= saveEntry cmd . fromMaybe ""
