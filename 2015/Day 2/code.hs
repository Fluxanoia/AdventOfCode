import Data.List.Split
import Flux.Core

task_one :: [String] -> Int
task_one str = sum $ map (\area -> 2 * (sum area) + (minimum area)) areas 
    where 
        areas = map (\side -> map (foldr (*) 1) side) sides
        sides = map ((nsets 2)) dims
        dims = map (\dim -> map (\i -> read i::Int) (splitOn "x" dim)) str

task_two :: [String] -> Int
task_two str = sum $ map (\(p, v) -> p + v) (zip min_perims volumes)
    where 
        min_perims = map (\perim -> 2 * (minimum perim)) perims
        perims = map (\side -> map sum side) sides
        sides = map ((nsets 2)) dims
        volumes = map (foldr (*) 1) dims
        dims = map (\dim -> map (\i -> read i::Int) (splitOn "x" dim)) str

main :: IO ()
main = flux_main id task_one task_two
