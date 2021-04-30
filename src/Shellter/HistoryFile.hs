{-# LANGUAGE LambdaCase #-}

module Shellter.HistoryFile
  ( readHistoryFile,
    writeHistoryFile,
    HistoryEntry (..),
  )
where

import Control.Exception
import Data.CSV
import qualified Data.Either as DE
import Data.Functor
import Data.List
import Data.List.Split
import Data.List.Utils
import System.Directory
import qualified System.IO.Strict as SIO
import System.Posix.Files
import qualified Text.ParserCombinators.Parsec as PS

data HistoryEntry = HistoryEntry
  { projectPath :: String,
    cmd :: String,
    hits :: Int,
    lastUsed :: String
  }

instance Eq HistoryEntry where
  (==) a b = projectPath a == projectPath b && cmd a == cmd b

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

readHistoryFile' :: IO String
readHistoryFile' =
  ( getConfigFilePath
      >>= (try . SIO.readFile :: String -> IO (Either IOException String))
  )
    <&> DE.fromRight ""

parseParts :: [String] -> HistoryEntry
parseParts [path, cmd, hits, lastUsed] =
  HistoryEntry
    path
    cmd
    (Prelude.read hits :: Int)
    lastUsed

parseHistoryEntries :: [[String]] -> [HistoryEntry]
parseHistoryEntries = map parseParts

-- TODO: In fact IO (Either ParseError [HistoryEntry]) should be returned,
-- because it clears out if something isn't right in the format
readHistoryFile :: IO [HistoryEntry]
readHistoryFile = parseHistoryEntries . DE.fromRight [] . PS.parse csvFile "" <$> readHistoryFile'

processToSave :: [HistoryEntry] -> [[String]]
processToSave = map (\entry -> [projectPath entry, cmd entry, show $ hits entry, lastUsed entry])

genOutputCsv :: [HistoryEntry] -> String
genOutputCsv = genCsvFile . processToSave

writeHistoryFile :: [HistoryEntry] -> IO ()
writeHistoryFile x = getConfigFilePath >>= (`writeFile` genOutputCsv x)
