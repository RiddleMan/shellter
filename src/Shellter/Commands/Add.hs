{-# LANGUAGE LambdaCase #-}

module Shellter.Commands.Add where

import Shellter.Types
import System.Directory

configFilePath :: String
configFilePath = "~/.shellter_history"

hasGitFolder :: FilePath -> IO Bool
hasGitFolder path = doesPathExist $ path ++ "/.git"

findProjectRoot' :: String -> IO (Maybe String)
findProjectRoot' "/" = return Nothing
findProjectRoot' currPath =
  hasGitFolder currPath >>=
  (\case
     True -> return (Just currPath)
     False -> canonicalizePath (currPath ++ "/../") >>= findProjectRoot')

run :: String -> String -> IO ()
run path cmd =
  print ("path: " ++ path) >> print ("cmd: " ++ cmd) >>
  (findProjectRoot' path >>= print)
