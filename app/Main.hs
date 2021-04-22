module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import Shellter

{--
data Sample =
  Sample
    { hello :: String
    , quiet :: Bool
    , enthusiasm :: Int
    }

sample :: Parser Sample
sample =
  Sample <$>
  strOption (long "hello" <> metavar "TARGET" <> help "Target for the greeting") <*>
  switch (long "quiet" <> short 'q' <> help "Whether to be quiet") <*>
  option
    auto
    (long "enthusiasm" <>
     help "How enthusiastically to greet" <>
     showDefault <> value 1 <> metavar "INT")

main :: IO ()
main = greet =<< execParser opts
  where
    opts =
      info
        (sample <**> helper)
        (fullDesc <>
         progDesc "Print a greeting for TARGET" <>
         header "hello - a test for optparse-applicative")

greet :: Sample -> IO ()
greet (Sample h False n) = putStrLn $ "Hello, " ++ h ++ replicate n '!'
greet _ = return ()
--}
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

addParser :: Parser Command
addParser =
  Add <$> argument str (metavar "path") <*> argument str (metavar "command")

subCommandParser :: Parser Command
subCommandParser =
  hsubparser
    (command "add" (info addParser (progDesc "Add command to history")))

commandParser :: Parser Options
commandParser =
  Options <$> subCommandParser <*>
  strOption
    (long "test" <> metavar "TARGET" <> help "Test asdf" <> value "asdf") <*>
  strOption
    (long "test2" <> metavar "TARGET" <> help "Test 2 asdf" <> value "asdf2")

run :: Options -> IO ()
run = print

main :: IO ()
main = execParser opt >>= run
  where
    opt =
      info
        (commandParser <**> helper)
        (fullDesc <>
         progDesc "Example for multiple subcommands" <> header "myProgram")
