{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Shellter.Commands.Add where

import Data.Maybe
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

saveEntry :: String -> String -> IO ()
saveEntry cmd path =
  readHistoryFile
    >>= writeHistoryFile
      . map
        ( \t ->
            if projectPath t == path
              then t
              else t {hits = hits t + 1}
        )

run :: String -> String -> IO ()
run path cmd = findProjectRoot path >>= saveEntry cmd . fromMaybe "" --saveEntry cmd . fromMaybe "" <$> findProjectRoot path
