import Lib (allCombinations)
import Data.List (intersperse)

main :: IO ()
main = do
  let cs = allCombinations "(static, event_a|event_b (old|new, red|green))"
  putStrLn $ foldl (++) "" $ intersperse "\n" cs

