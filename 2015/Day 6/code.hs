import Data.List.Split
import Flux.Core

type Grid = [((Int, Int), Int)]

start_grid :: Grid
start_grid = [((x, y), 0) | x <- [0..999], y <- [0..999]]

get_trans_one :: String -> (Int -> Int)
get_trans_one "on"     = (\_ -> 1)
get_trans_one "off"    = (\_ -> 0)
get_trans_one "toggle" = (\i -> ternary (i == 1) 0 1)

get_trans_two :: String -> (Int -> Int)
get_trans_two "on"     = (\i -> i + 1)
get_trans_two "off"    = (\i -> ternary (i == 0) 0 (i - 1))
get_trans_two "toggle" = (\i -> i + 2)

transition :: Grid -> (Int -> Int) -> (Int, Int) -> (Int, Int) -> Grid
transition [] _ _ _ = []
transition (((x, y), bri):xs) trans (a, b) (p, q)
    | is_in     = ((x, y), trans bri):rest
    | otherwise = ((x, y), bri):rest
    where
        is_in = (x >= a && x <= p) && (y >= b && y <= q)
        rest = transition xs trans (a, b) (p, q)

full_transition :: Grid -> (String -> (Int -> Int)) -> [String] -> Grid
full_transition grid gt []     = grid
full_transition grid gt (x:xs) = full_transition new gt xs
    where
        new = transition grid trans (head start, last start) (head end, last end)
        trans = gt (head $ (drop 3 split))
        start = map read (splitOn "," (head $ (drop 2 split))) :: [Int]
        end = map read (splitOn "," (head $ split)) :: [Int]
        split = reverse $ splitOn " " x

lights_on :: Grid -> Int
lights_on grid = sum $ (map (\(_, i) -> i) grid)

task_one :: [String] -> Int
task_one str = lights_on $ full_transition start_grid get_trans_one str

task_two :: [String] -> Int
task_two str = lights_on $ full_transition start_grid get_trans_two str

main :: IO ()
main = flux_main id task_one task_two
