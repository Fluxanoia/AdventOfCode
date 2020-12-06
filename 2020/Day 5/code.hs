import Flux.Core

to_binary :: String -> [Int]
to_binary = map tb
    where 
        tb :: Char -> Int
        tb 'F' = 0
        tb 'L' = 0
        tb 'B' = 1
        tb 'R' = 1
        tb _   = -1

get_seat :: String -> Int
get_seat str = row * 8 + col
    where
        row = sum $ zipWith (*) pows $ to_binary $ reverse $ take 7 str
        col = sum $ zipWith (*) pows $ to_binary $ reverse $ take_end 3 str
        pows = [2 ^ x | x <- [0..6]]

task_one :: [String] -> [Int]
task_one strs = [maximum $ map get_seat strs]

task_two :: [String] -> [Int]
task_two strs = filter (\i -> not $ elem i seats) [0..1023]
    where 
        seats = map get_seat strs

main :: IO ()
main = flux_main id task_one task_two
