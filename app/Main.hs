module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import Shellter

addParser :: Parser Command
addParser =
  Add <$> argument str (metavar "PATH")
    <*> argument str (metavar "COMMAND")
    <*> argument auto (metavar "ERROR_CODE")

subCommandParser :: Parser Command
subCommandParser =
  hsubparser
    (command "add" (info addParser (progDesc "Add command to history")))

commandParser :: Parser Options
commandParser =
  Options <$> subCommandParser
    <*> strOption
      (long "test" <> metavar "TARGET" <> help "Test asdf" <> value "asdf")
    <*> strOption
      (long "test2" <> metavar "TARGET" <> help "Test 2 asdf" <> value "asdf2")

main :: IO ()
main = execParser opt >>= run
  where
    opt =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "Example for multiple subcommands"
            <> header "myProgram"
        )
