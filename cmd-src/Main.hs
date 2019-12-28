import Lib (allCombinations, testParse)
import Language.States.Options

import Options.Applicative (execParser)

import System.IO
import Control.Monad (void)

main :: IO ()
main = do
  opts <- execParser cmdLineOptions
  defs <- getStateDefinition (optInput opts)
  -- testParse input
  let result = allCombinations defs (optExtraFilters opts)
  either putStrLn (mapM_ putStrLn) result

getStateDefinition :: InputStyle -> IO String
getStateDefinition (FromSource s) = return s
getStateDefinition FromStdin      = hGetContents stdin
getStateDefinition (FromFile f)   = readFile f

