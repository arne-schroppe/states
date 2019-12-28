import Lib (allCombinations, testParse)
import Language.States.Options

import Options.Applicative (execParser)

import System.IO
import Control.Monad (void)

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

