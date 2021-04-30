{-# LANGUAGE LambdaCase #-}

module Shellter.HistoryFile
  ( readHistoryFile,
    writeHistoryFile,
    HistoryEntry (..),
  )
where

import Control.Exception
import qualified Data.Either as DE
import Data.Functor
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

instance Eq HistoryEntry where
  (==) a b = projectPath a == projectPath b && cmd a == cmd b

-- TODO: escape/use different format to allow typing in ; in the command line
instance Show HistoryEntry where
  show entry =
    intercalate ";" [projectPath entry, cmd entry, show (hits entry), lastUsed entry]

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
    <$> ( ( getConfigFilePath
              >>= (try . SIO.readFile :: String -> IO (Either IOException String))
          )
            <&> DE.fromRight ""
        )

parseParts :: [String] -> HistoryEntry
parseParts [path, cmd, hits, lastUsed] =
  HistoryEntry
    path
    cmd
    (Prelude.read hits :: Int)
    lastUsed

-- TODO: Rewrite to the instance of the Read typeclass
parseHistoryLine :: String -> [HistoryEntry]
parseHistoryLine =
  (\parts -> ([parseParts parts | length parts == 4]))
    . splitOn ";"

parseHistoryEntries :: [String] -> [HistoryEntry]
parseHistoryEntries arr =
  filter
    ( \case
        "\n" -> False
        _ -> True
    )
    arr
    >>= parseHistoryLine

readHistoryFile :: IO [HistoryEntry]
readHistoryFile = parseHistoryEntries <$> readHistoryFile'

writeHistoryFile :: [HistoryEntry] -> IO ()
writeHistoryFile x = getConfigFilePath >>= (`writeFile` (intercalate "\n" . map show) x)
