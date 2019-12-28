import Lib (allCombinations, testParse)

import Options.Applicative

import System.IO
import System.Environment (getArgs)
import Data.List (intersperse)
import Data.Maybe (maybe)
import Control.Monad (void, unless)
import Data.Semigroup ((<>))


data Options = Options
  { optInput :: Maybe String
  }
  deriving (Show)

main :: IO ()
main = do
  opts <- execParser cmdLineOptions
  let inputOption = optInput opts
  input <- maybe (hGetContents stdin) return inputOption
  -- testParse input
  let result = allCombinations input
  case result of
    Left err -> putStrLn err
    Right cs -> void $ mapM putStrLn cs

cmdLineOptions :: ParserInfo Options
cmdLineOptions = info (optionsParser <**> helper)
  ( fullDesc
  <> progDesc "Print all combinations of states"
  <> header "states - a tool to explore states" )

optionsParser :: Parser Options
optionsParser = Options
      <$> optional (argument str
          ( metavar "SOURCE"
         <> help "The state definition" ))
