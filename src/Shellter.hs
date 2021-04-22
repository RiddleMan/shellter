module Shellter
  ( run
  , Options(..)
  , Command(..)
  ) where

import Brick
import Shellter.Types

ui :: Widget ()
ui = str "Hello, world!"

run :: Options -> IO ()
run = print
