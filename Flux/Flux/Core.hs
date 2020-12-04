module Flux.Core where

import Data.Set (toList, fromList)

-- Helper Functions

ternary :: Bool -> a -> a -> a
ternary b x y
    | b         = x
    | otherwise = y

-- Type Conversion

bti :: Bool -> Int
bti True  = 1
bti False = 0

sti :: String -> Int
sti = read

-- Input Validation

is_num :: String -> Bool
is_num str = foldr (&&) (True) (map (\x -> elem x ['0'..'9']) str)

range :: (Ord a, Num a) => a -> a -> a -> Bool
range l n u = n > l && n < u

-- List Operations

remove_dupes :: Ord a => [a] -> [a]
remove_dupes = toList . fromList

nsets :: Int -> [a] -> [[a]]
nsets _ []     = []
nsets 0 _      = []
nsets 1 xs     = (map (\x -> [x]) xs)
nsets n (x:xs) = (map ((:) x) (nsets (n - 1) xs)) ++ (nsets n xs)

take_end :: Int -> [a] -> [a]
take_end i xs 
    | i > 0     = (take_end (i - 1) (lose 1 xs)) ++ [last xs]
    | otherwise = []

lose :: Int -> [a] -> [a]
lose i xs
    | i > 0     = lose (i - 1) (init xs)
    | otherwise = xs

-- Input Options and Main

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