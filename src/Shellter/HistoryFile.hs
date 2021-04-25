module Shellter.HistoryFile (saveEntry) where

import Data.Time

data HistoryEntry = HistoryEntry
  { path :: FilePath,
    projectPath :: String,
    cmd :: String,
    hits :: Int,
    lastUsed :: String
  }

instance Show HistoryEntry where show entry = path entry ++ ":" ++ projectPath entry ++ ":" ++ cmd entry ++ ":" ++ show (hits entry) ++ ":" ++ lastUsed entry

configFilePath :: String
configFilePath = "~/.shellter_history"

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

saveEntry :: String -> String -> String -> IO ()
saveEntry path projectPath cmd =
  getCurrentTime
    >>= ( \time ->
            appendFile
              configFilePath
              ( show
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
