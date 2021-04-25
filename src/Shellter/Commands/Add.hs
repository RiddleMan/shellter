{-# LANGUAGE LambdaCase #-}

module Shellter.Commands.Add where

import Data.Maybe
import Shellter.HistoryFile
import Shellter.Types
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

run :: String -> String -> IO ()
run path cmd = findProjectRoot path >>= saveEntry path cmd . fromMaybe ""
