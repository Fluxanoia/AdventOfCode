import Data.List.Split
import Flux.Core

type BagInfo = (String, [(String, Int)])

input_trans :: [String] -> [BagInfo]
input_trans = map s_map
    where
        get_bag :: String -> String
        get_bag s = drop_end 5 $ (splitOn " contain " s) !! 0

        get_req :: String -> [String]
        get_req s = splitOn ", " $ drop_end 1 $ (splitOn " contain " s) !! 1

        s_map :: String -> BagInfo
        s_map s
            | "no other bags" == (head $ reqs) = (get_bag s, [])
            | otherwise                        = (get_bag s, map (\r -> (get_req_str r, get_req_int r)) reqs)
            where reqs = get_req s

        get_req_int :: String -> Int
        get_req_int r = sti $ (splitOn " " r) !! 0

        get_req_str :: String -> String
        get_req_str r
            | last r == 's' = drop_end 5 raw 
            | otherwise     = drop_end 4 raw
            where raw = drop (1 + (length $ show $ get_req_int r)) r

contained_in :: [String] -> [BagInfo] -> [String]
contained_in xs bags = map fst $ filter (\(_, cs) -> (length $ intersection [xs, map fst cs]) /= 0) bags

all_that_contain :: [String] -> [BagInfo] -> [String]
all_that_contain [] bags = []
all_that_contain xs bags = remove_dupes $ (all_that_contain conts bags) ++ conts
    where conts = contained_in xs bags

must_contain :: String -> [BagInfo] -> [(String, Int)]
must_contain s bags = concat_all $ map snd $ filter (\(b, _) -> b == s) bags

needed_bags :: String -> [BagInfo] -> Int
needed_bags s bags = sum $ map (\(n, i) -> i + i * (needed_bags n bags)) $ must_contain s bags

task_one :: [BagInfo] -> Int
task_one info = length $ all_that_contain ["shiny gold"] info

task_two :: [BagInfo] -> Int
task_two = needed_bags "shiny gold"

main :: IO ()
main = flux_main input_trans task_one task_two 
