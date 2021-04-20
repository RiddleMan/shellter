module Shellter
  ( app
  ) where

import Brick

ui :: Widget ()
ui = str "Hello, world!"

app :: IO ()
app = simpleMain ui
