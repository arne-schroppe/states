import Lib (allCombinations)

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
  let cs = allCombinations input
  void $ mapM putStrLn cs

