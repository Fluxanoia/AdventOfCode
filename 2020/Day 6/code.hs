import Data.List.Split
import Flux.Core

task_two :: [[String]] -> Int
task_two strs = sum $ map (length . intersection) strs

task_one :: [[String]] -> Int
task_one strs = sum $ map (length . remove_dupes) groups
    where groups = map concat_all strs

main :: IO ()
main = flux_main (splitOn [""]) task_one task_two
