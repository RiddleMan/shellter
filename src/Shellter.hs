module Shellter
  ( run,
    Options (..),
    Command (..),
  )
where

import Brick
import qualified Shellter.Commands.Add as CmdAdd
import Shellter.Types

ui :: Widget ()
ui = str "Hello, world!"

run :: Options -> IO ()
run opt =
  case subCmd opt of
    Add path cmd errorCode -> CmdAdd.run path cmd errorCode
