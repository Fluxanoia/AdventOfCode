import Flux.Core

invalid_substrs :: [String]
invalid_substrs = ["ab", "cd", "pq", "xy"]

contains :: String -> String -> Bool
contains [] sub = False
contains (x:xs) sub
    | (take (length sub) (x:xs)) == sub = True
    | otherwise                          = contains xs sub

no_bad_substr :: String -> Bool
no_bad_substr str = not $ foldr (||) False tests 
    where
        tests = map (contains str) invalid_substrs

contains_double :: String -> Bool
contains_double []     = False
contains_double (x:[]) = False
contains_double (x:y:xs)
    | x == y    = True
    | otherwise = contains_double (y:xs)

good_vowels :: String -> Bool
good_vowels str = 2 < length (filter (\c -> elem c "aeiou") str)

is_nice :: String -> Bool
is_nice str = (good_vowels str) && (contains_double str) && (no_bad_substr str)

contains_repeat_with_space :: String -> Bool
contains_repeat_with_space []       = False
contains_repeat_with_space (x:[])   = False
contains_repeat_with_space (x:y:[]) = False
contains_repeat_with_space (x:y:z:xs)
    | x == z    = True
    | otherwise = contains_repeat_with_space (y:z:xs)

contains_double_double :: String -> Bool
contains_double_double []     = False
contains_double_double (x:[]) = False
contains_double_double (x:y:xs)
    | contains xs (x:y:[]) = True
    | otherwise            = contains_double_double (y:xs)

is_actually_nice :: String -> Bool
is_actually_nice str = (contains_double_double str) && (contains_repeat_with_space str)

task_one :: [String] -> Int
task_one input = sum $ map bti (map is_nice input)

task_two :: [String] -> Int
task_two input = sum $ map bti (map is_actually_nice input)

main :: IO ()
main = flux_main id task_one task_two
