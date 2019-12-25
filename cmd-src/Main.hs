import Lib (allCombinations, testParse)

import System.IO
import System.Environment (getArgs)
import Data.List (intersperse)
import Control.Monad (void, unless)


main :: IO ()
main = do
  args <- getArgs
  input <- if (length args == 1)
    then return (head args)
    else hGetContents stdin
  -- testParse input
  let result = allCombinations input
  case result of
    Left err -> putStrLn err
    Right cs -> void $ mapM putStrLn cs

