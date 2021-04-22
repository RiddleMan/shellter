module Shellter.Types
  ( Command(..)
  , Options(..)
  ) where

data Command =
  Add
    { path :: FilePath
    , cmd :: String
    }
  deriving (Show)

data Options =
  Options
    { subCmd :: Command
    , test :: String
    , test2 :: String
    }
  deriving (Show)
