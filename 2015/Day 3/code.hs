import Data.List.Split
import Flux.Core

type Grid = [((Int, Int), Int)]

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) '^' = (x, y + 1)
move (x, y) 'v' = (x, y - 1)
move (x, y) '<' = (x + 1, y)
move (x, y) '>' = (x - 1, y)
move p _        = p

deliver :: Grid -> (Int, Int) -> Grid
deliver [] pos = [(pos, 1)]
deliver ((cell, p):xs) pos
    | cell == pos = (cell, p + 1):xs
    | otherwise   = (cell, p):(deliver xs pos)

deliver_all :: String -> Grid -> (Int, Int) -> Grid
deliver_all [] grid _       = grid
deliver_all (x:xs) grid pos = deliver_all xs (deliver grid new_pos) new_pos
    where new_pos = move pos x

task_one :: String -> Int
task_one str = length $ deliver_all str [((0, 0), 1)] (0, 0)

my_divvy :: String -> (String, String)
my_divvy []       = ("", "")
my_divvy (x:[])   = ([x], "")
my_divvy (x:y:xs) = (x:nx, y:ny)
    where (nx, ny) = my_divvy xs

task_two :: String -> Int
task_two str = length robo_grid
    where 
        robo_grid = deliver_all robo santa_grid (0, 0)
        santa_grid = deliver_all santa [((0, 0), 1)] (0, 0)
        (santa, robo) = my_divvy str

main :: IO ()
main = flux_main input_str task_one task_two
