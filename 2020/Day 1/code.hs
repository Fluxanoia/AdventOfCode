import Flux.Core

task :: Int -> [Int] -> Int
task n xs = foldr (*) 1 list
    where list = head $ filter (\x -> sum x == 2020) (nsets n xs)

main :: IO ()
main = flux_main input_ints (task 2) (task 3)
