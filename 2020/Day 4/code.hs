import Data.List.Split
import Flux.Core

-- Task Two

check_hair :: String -> Bool
check_hair ('#':hcl) = (length hcl) == 6
    && (foldr (&&) True 
    $ map (\c -> elem c $ ['0'..'9'] ++ ['a'..'f']) hcl)
check_hair _         = False

check_height :: String -> Bool
check_height s
    | end == "in" = 58 < start && 77 > start
    | end == "cm" = 149 < start && 194 > start
    | otherwise  = False
    where 
        start = sti $ lose 2 s
        end = take_end 2 s

check :: (String, String) -> Bool
check ("byr", d) = range 1919 (sti d) 2003 
check ("iyr", d) = range 2009 (sti d) 2021 
check ("eyr", d) = range 2019 (sti d) 2031 
check ("hgt", d) = check_height d
check ("hcl", d) = check_hair d
check ("ecl", d) = elem d ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
check ("pid", d) = (is_num d) && (length d) == 9
check (f, _)     = not $ elem f needed_fields

is_valid_two :: [[(String, String)]] -> [Bool]
is_valid_two []     = []
is_valid_two (x:xs) = (foldr (&&) True $ map check x):(is_valid_two xs)

-- Task One

needed_fields :: [String]
needed_fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

is_valid_one :: [String] -> Bool
is_valid_one l = foldr (&&) True (map (\f -> elem f l) needed_fields)

-- Data Collection

get_entries :: [String] -> [String]
get_entries strs = splitOn "|" $ foldl f "" strs
    where
        f :: String -> String -> String
        f b a = ternary ((length a) == 0) (b ++ "|") (b ++ " " ++ a)

get_fields :: [String] -> [[String]]
get_fields strs = map (map $ take_end 3) raw_fields
    where 
        raw_fields = map ((lose 1) . (splitOn ":")) $ get_entries strs

get_data :: [String] -> [[String]]
get_data strs = map (\l -> (map (lose 4) (lose 1 l)) ++ [last l]) raw_data
    where 
        raw_data = map ((drop 1) . (splitOn ":")) $ get_entries strs

-- Main Functions

task_one :: [String] -> Int
task_one strs = sum $ map bti (map is_valid_one $ get_fields strs)

task_two :: [String] -> Int
task_two strs = sum $ map bti $ zipWith (&&) valid_one valid_two
    where
        valid_one = map is_valid_one fields
        valid_two = is_valid_two $ zipWith zip fields $ get_data strs
        fields = get_fields strs

main :: IO ()
main = flux_main (map (lose 1)) task_one task_two
