import Lib (allCombinations, testParse)

import Options.Applicative

import System.IO
import System.Environment (getArgs)
import Data.List (intersperse)
import Data.Maybe (maybe)
import Control.Monad (void, unless)
import Data.Semigroup ((<>))
import Data.Functor ((<&>))

data InputStyle =
    FromSource String
  | FromStdin
  | FromFile String
  deriving (Show)

data Options = Options
  { optInput        :: InputStyle
  , optExtraFilters :: String
  }
  deriving (Show)

main :: IO ()
main = do
  opts <- execParser cmdLineOptions
  input <- getStateDefinition (optInput opts)
  -- testParse input
  let result = allCombinations input (optExtraFilters opts)
  case result of
    Left err -> putStrLn err
    Right cs -> void $ mapM putStrLn cs

getStateDefinition :: InputStyle -> IO String
getStateDefinition (FromSource s) = return s
getStateDefinition FromStdin      = hGetContents stdin
getStateDefinition (FromFile f)   = readFile f

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

