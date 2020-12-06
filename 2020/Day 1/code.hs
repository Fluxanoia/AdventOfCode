import Flux.Core

task :: Int -> [Int] -> Int
task n xs = product $ head $ filter (\x -> sum x == 2020) (nsets n xs)

main :: IO ()
main = flux_main sti_all (task 2) (task 3)
