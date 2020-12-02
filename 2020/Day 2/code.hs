import Data.List.Split

is_valid :: String -> Bool
is_valid str = num >= l && num <= u
    where 
        l = read (head $ splitOn "-" (head split))::Int
        u = read (last $ splitOn "-" (head split))::Int
        num = length $ filter ((==) letter) word
        letter = head (split !! 1)
        word = last split
        split = splitOn " " str

is_actually_valid :: String -> Bool
is_actually_valid str = (has_l || has_u) && not (has_l && has_u)
    where 
        has_l = (head $ drop (l - 1) word) == letter
        has_u = (head $ drop (u - 1) word) == letter
        l = read (head $ splitOn "-" (head split))::Int
        u = read (last $ splitOn "-" (head split))::Int
        letter = head (split !! 1)
        word = last split
        split = splitOn " " str

task_one :: [String] -> Int
task_one str = length $ filter id (map is_valid str)

task_two :: [String] -> Int
task_two str = length $ filter id (map is_actually_valid str)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let input = lines contents :: [String]
  print ("Task One: " ++ (show $ task_one input))
  print ("Task Two: " ++ (show $ task_two input))
