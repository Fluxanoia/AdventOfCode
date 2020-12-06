module Flux.Core where

import Data.Set (toList, fromList)

-- Helper Functions

ternary :: Bool -> a -> a -> a
ternary b x y
    | b         = x
    | otherwise = y

concat_all :: [[a]] -> [a]
concat_all = foldr (++) []

and_all :: [Bool] -> Bool
and_all = foldr (&&) True

or_all :: [Bool] -> Bool
or_all = foldr (||) False

sti_all :: [String] -> [Int]
sti_all = map sti

-- Type Conversion

bti :: Bool -> Int
bti True  = 1
bti False = 0

sti :: String -> Int
sti = read

-- Input Validation

is_num :: String -> Bool
is_num str = and_all $ map (\x -> elem x ['0'..'9']) str

range :: (Ord a, Num a) => a -> a -> a -> Bool
range l n u = n > l && n < u

-- String Operations

has_substr :: String -> String -> Bool
has_substr [] sub = False
has_substr (x:xs) sub
    | (take (length sub) (x:xs)) == sub = True
    | otherwise                          = has_substr xs sub

-- List Operations

remove_dupes :: Ord a => [a] -> [a]
remove_dupes = toList . fromList

nsets :: Int -> [a] -> [[a]]
nsets _ []     = []
nsets 0 _      = []
nsets 1 xs     = (map (\x -> [x]) xs)
nsets n (x:xs) = (map ((:) x) (nsets (n - 1) xs)) ++ (nsets n xs)

take_end :: Int -> [a] -> [a]
take_end i xs = reverse $ take i $ reverse xs

drop_end :: Int -> [a] -> [a]
drop_end i xs = reverse $ drop i $ reverse xs

filter_by :: [Bool] -> [a] -> [a]
filter_by [] _ = []
filter_by _ [] = []
filter_by (b:bs) (x:xs)
    | b         = x:rest
    | otherwise = rest
    where rest = filter_by bs xs

intersection :: Ord a => [[a]] -> [a]
intersection lists = filter_by bools elems
    where 
        bools = map (\x -> elem_of_all x lists) elems
        elems = remove_dupes $ concat_all lists

        elem_of_all :: Eq a => a -> [[a]] -> Bool
        elem_of_all x xss = and_all $ map (\xs -> elem x xs) xss

-- Main

flux_main :: (Show b, Show c) => ([String] -> a) -> (a -> b) -> (a -> c) -> IO ()
flux_main input_trans task_one task_two = do
  contents <- readFile "input.txt"
  let input = input_trans $ (map $ drop_end 1) $ lines contents
  print ("Task One: " ++ (show $ task_one input))
  print ("Task Two: " ++ (show $ task_two input))