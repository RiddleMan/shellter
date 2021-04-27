{-# LANGUAGE LambdaCase #-}

module Shellter.HistoryFile
  ( readHistoryFile,
    writeHistoryFile,
    HistoryEntry (..),
  )
where

import Control.Exception
import qualified Data.Either as DE
import Data.List
import Data.List.Split
import System.Directory
import qualified System.IO.Strict as SIO
import System.Posix.Files

data HistoryEntry = HistoryEntry
  { projectPath :: String,
    cmd :: String,
    hits :: Int,
    lastUsed :: String
  }

instance Show HistoryEntry where
  show entry =
    projectPath entry
      ++ ":"
      ++ cmd entry
      ++ ":"
      ++ show (hits entry)
      ++ ":"
      ++ lastUsed entry

getConfigFilePath :: IO FilePath
getConfigFilePath = (++ "/.shellter_history") <$> getHomeDirectory

createIfDoesNotExist :: IO ()
createIfDoesNotExist =
  getConfigFilePath
    >>= doesPathExist
    >>= ( \case
            True -> pure ()
            False -> getConfigFilePath >>= (`writeFile` "")
        )

readHistoryFile' :: IO [String]
readHistoryFile' =
  lines
    <$> ( getConfigFilePath
            >>= (try . SIO.readFile :: String -> IO (Either IOException String))
            >>= pure . DE.fromRight ""
        )

-- TODO: Rewrite to the instance of the Read typeclass
-- TODO: Check whether we can parse hits or any entries in the file
parseHistoryLine :: String -> HistoryEntry
parseHistoryLine =
  ( \[path, cmd, hits, lastUsed] ->
      HistoryEntry
        path
        cmd
        (Prelude.read hits :: Int)
        lastUsed
  )
    . splitOn ":"

parseHistoryEntries :: [String] -> [HistoryEntry]
parseHistoryEntries =
  map parseHistoryLine
    . filter
      ( \case
          "\n" -> False
          _ -> True
      )

readHistoryFile :: IO [HistoryEntry]
readHistoryFile = parseHistoryEntries <$> readHistoryFile'

writeHistoryFile :: [HistoryEntry] -> IO ()
writeHistoryFile x = getConfigFilePath >>= (`writeFile` (intercalate "\n" . map show) x)
