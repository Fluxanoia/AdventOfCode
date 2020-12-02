module Flux.Core where

bti :: Bool -> Int
bti True  = 1
bti False = 0

nsets :: Int -> [a] -> [[a]]
nsets _ []     = []
nsets 0 _      = []
nsets 1 xs     = (map (\x -> [x]) xs)
nsets n (x:xs) = (map ((:) x) (nsets (n - 1) xs)) ++ (nsets n xs)

input_str :: [String] -> String
input_str = foldr (++) []

input_ints :: [String] -> [Int]
input_ints = map read

flux_main :: (Show b, Show c) => ([String] -> a) -> (a -> b) -> (a -> c) -> IO ()
flux_main input_trans task_one task_two = do
  contents <- readFile "input.txt"
  let input = input_trans $ lines contents
  print ("Task One: " ++ (show $ task_one input))
  print ("Task Two: " ++ (show $ task_two input))