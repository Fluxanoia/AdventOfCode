import Data.List.Split
import Flux.Core

get_num :: String -> Int
get_num ('+':s) = sti s
get_num s       = sti s

run_cmd_flipped :: String -> Int -> Int -> (Int, Int)
run_cmd_flipped cmd i n
    | op == "nop" = (i + num, n)
    | op == "jmp" = (i + 1, n)
    | op == "acc" = (i + 1, n + num)
    | otherwise   = error $ "bad op " ++ op
    where
        op = (splitOn " " cmd) !! 0
        num = get_num $ (splitOn " " cmd) !! 1

run_cmd :: String -> Int -> Int -> (Int, Int)
run_cmd cmd i n
    | op == "nop" = (i + 1, n)
    | op == "jmp" = (i + num, n)
    | op == "acc" = (i + 1, n + num)
    | otherwise   = error $ "bad op " ++ op
    where
        op = (splitOn " " cmd) !! 0
        num = get_num $ (splitOn " " cmd) !! 1

run_inf :: [String] -> [Int] -> Int -> Int -> Int
run_inf cmds ran i n
    | i >= length cmds = n
    | elem i ran       = n
    | otherwise        = run_inf cmds (i:ran) ni nn
    where (ni, nn) = run_cmd (cmds !! i) i n

run_trm :: [String] -> [Int] -> Int -> Int -> Int -> (Int, Bool)
run_trm cmds ran flipped i n
    | i == length cmds = (n, True)
    | elem i ran       = (n, False)
    | otherwise        = run_trm cmds (i:ran) flipped ni nn
    where 
        (ni, nn) = f (cmds !! i) i n
        f = ternary (i == flipped) run_cmd_flipped run_cmd

task_one :: [String] -> Int
task_one strs = run_inf strs [] 0 0

task_two :: [String] -> [(Int, Bool)]
task_two strs = fst $ head $ filter (\(_, b) -> b) $ attempts
    where attempts = map (\i -> run_trm strs [] i 0 0) [0..(length strs) - 1]

main :: IO ()
main = flux_main id task_one task_two 
