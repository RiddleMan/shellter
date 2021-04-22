module Shellter.Commands.Add where

import Shellter.Types

run :: String -> String -> IO ()
run path cmd = print ("path: " ++ path) >> print ("cmd: " ++ cmd)
