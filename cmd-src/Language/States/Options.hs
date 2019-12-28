module Language.States.Options (
  Options(..)
, InputStyle(..)
, cmdLineOptions
) where

import Options.Applicative

import Data.Maybe (maybe)
import Data.Semigroup ((<>))
import Data.Functor ((<&>))

data Options = Options
  { optInput        :: InputStyle
  , optExtraFilters :: String
  }
  deriving (Show)

data InputStyle =
    FromSource String
  | FromStdin
  | FromFile String
  deriving (Show)

cmdLineOptions :: ParserInfo Options
cmdLineOptions = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "Print all combinations of states"
  <> header "states - a tool to explore states" )

optionsParser :: Parser Options
optionsParser = Options
      <$> inputOptionParser
      <*> strOption
          ( long "filters"
         <> short 'F'
         <> metavar "FILTER-SRC"
         <> value ""
         <> help "Additional filters to apply" )

inputOptionParser :: Parser InputStyle
inputOptionParser = sourceOrStdinOptionParser <|> fileInputOptionParser

sourceOrStdinOptionParser :: Parser InputStyle
sourceOrStdinOptionParser =
          (optional (argument str
          ( metavar "SOURCE"
         <> help "The state definition" ))) <&>
          (maybe FromStdin FromSource)

fileInputOptionParser :: Parser InputStyle
fileInputOptionParser =
          strOption
          ( long "file"
         <> short 'f'
         <> metavar "FILE"
         <> action "file"
         <> help "Read definitions from FILE" ) <&>
          FromFile

