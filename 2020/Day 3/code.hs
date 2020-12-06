import Flux.Core

gridify :: [String] -> [[Bool]]
gridify []     = [[]]
gridify (x:xs) = (map ((==) '#') x):(gridify xs)

task :: (Int, Int) -> [String] -> Int
task (dx, dy) strs = sum $ map (\(x, y) -> bti ((grid !! y) !! x)) pos
    where
        grid = gridify strs
        pos = [(mod (d * dx) width, mod (d * dy) height) | d <- [0..height], d * dy < height]
        width = length $ head strs
        height = length strs

task_one :: [String] -> Int
task_one = task (3, 1)

task_two :: [String] -> Int
task_two strs = foldr (*) 1 (map (\d -> task d strs) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)])

main :: IO ()
main = flux_main id task_one task_two
