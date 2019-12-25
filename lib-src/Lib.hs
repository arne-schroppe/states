module Lib (
  allCombinations
, testParse
) where

import Language.States.Types
import Language.States.Parser (parse, testParse)
import Language.States.Denormaliser (denormalise)
import Language.States.Combinations (filteredCombinations)

import Data.Maybe (maybeToList)
import Data.List (intersperse)
import Control.Monad (void)

allCombinations :: String -> [String]
allCombinations src =
  let src' = removeComments src in
  let expr = parse src' >>= denormalise in
  case expr of
    Left err -> [err]
    Right expr -> let combs = filteredCombinations expr in
                  map prettyPrint combs

-- TODO figure out which regex library is good and use regexes instead
removeComments :: String -> String
removeComments src =
  remCom False "" src
  where
    remCom :: Bool -> String -> String -> String
    remCom _     processed []          = reverse processed
    remCom False processed ('#':rest)  = remCom True processed rest
    remCom True  processed ('\n':rest) = remCom False processed rest
    remCom True  processed (_:rest)    = remCom True processed rest
    remCom False processed (c:rest)    = remCom False (c:processed) rest


prettyPrint :: Value -> String
prettyPrint val = case val of
  VTuple (v:[]) -> prettyPrint v
  VTuple vals   -> "(" ++ concat (intersperse ", " (map prettyPrint vals)) ++ ")"
  VVariant s e  -> s ++ maybe "" (\x -> " " ++ prettyPrint x) e

