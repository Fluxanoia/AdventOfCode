import Flux.Core

preamble :: Int
preamble = 25

contiguous_sets :: [Int] -> Int -> [[Int]]
contiguous_sets ints len
    | length ints >= len = (take len ints):(contiguous_sets (drop 1 ints) len)
    | otherwise          = []

sums_contiguous :: [Int] -> Int -> Int -> [Int]
sums_contiguous ints target len
    | length sols > 0 = head $ sols
    | otherwise       = sums_contiguous ints target (len + 1)
    where sols = filter (\n -> sum n == target) $ contig_sets ints len

sums_to :: [Int] -> Int -> [[Int]]
sums_to ints i = filter (\n -> sum n == (ints !! i)) (nsets 2 $ list)
    where 
        list = drop to_drop $ take i ints
        to_drop = max 0 (i - preamble)

has_no_sum :: [Int] -> Int -> Int
has_no_sum ints i
    | (length $ sums_to ints i) > 0 = has_no_sum ints (i + 1)
    | otherwise                     = i

task_one :: [Int] -> Int
task_one ints = ints !! (has_no_sum ints preamble)

task_two :: [Int] -> Int
task_two ints = foldr1 min sol + foldr1 max sol
    where sol = sums_contiguous ints (task_one ints) 2

main :: IO ()
main = flux_main (map sti) task_one task_two 
