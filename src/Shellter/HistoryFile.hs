{-# LANGUAGE LambdaCase #-}

module Shellter.HistoryFile (saveEntry) where

import Data.Time
import System.Directory
import System.Posix.Files

data HistoryEntry = HistoryEntry
  { path :: FilePath,
    projectPath :: String,
    cmd :: String,
    hits :: Int,
    lastUsed :: String
  }

instance Show HistoryEntry where show entry = path entry ++ ":" ++ projectPath entry ++ ":" ++ cmd entry ++ ":" ++ show (hits entry) ++ ":" ++ lastUsed entry

getConfigFilePath :: IO FilePath
getConfigFilePath = (++ "/.shellter_history") <$> getHomeDirectory

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

createIfDoesNotExist :: IO ()
createIfDoesNotExist =
  getConfigFilePath
    >>= doesPathExist
    >>= ( \case
            True -> pure ()
            False -> getConfigFilePath >>= (`writeFile` "")
        )

saveEntry :: String -> String -> String -> IO ()
saveEntry path projectPath cmd =
  createIfDoesNotExist
    >> getCurrentTime
    >>= ( \time ->
            getConfigFilePath
              >>= ( `appendFile`
                      show
                        ( HistoryEntry
                            { path = path,
                              projectPath = projectPath,
                              cmd = cmd,
                              hits = 0,
                              lastUsed = formatDate time
                            }
                        )
                  )
        )
