import Data.Sort
import Flux.Core

jolt_diffs :: [Int] -> (Int, Int)
jolt_diffs ints = (snd $ foldl (j 1) (0, 0) ints, snd $ foldl (j 3) (0, 1) ints)
    where
        j :: Int -> (Int, Int) -> Int -> (Int, Int)
        j n (a, i) b
            | b - a == n = (b, i + 1)
            | otherwise  = (b, i) 

task_one :: [Int] -> Int
task_one ints = fst result * snd result
    where result = jolt_diffs ints

splitConsec :: [Int] -> [[Int]]
splitConsec [] = [[]]
splitConsec xs
    | length rest > 0 = consec:(splitConsec rest)
    | otherwise       = [consec]
    where
        consec = take (i + 1) xs
        rest = drop (i + 1) xs
        (_, i) = foldl f (head xs, 0) xs

        f :: (Int, Int) -> Int -> (Int, Int)
        f (a, i) b
            | b - a == 1 = (b, i + 1)
            | otherwise  = (a, i)

task_two :: [Int] -> Int
task_two ints = 0

main :: IO ()
main = flux_main (sort . map sti) task_one task_two
