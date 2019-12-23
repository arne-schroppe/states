import Lib (allCombinations)

import System.Environment (getArgs)
import Data.List (intersperse)
import Control.Monad (void, unless)

main :: IO ()
main = do
  args <- getArgs
  unless (length args == 0) $ do
    let cs = allCombinations $ head args
    void $ mapM putStrLn cs

