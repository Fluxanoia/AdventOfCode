import Flux.Core

task :: Int -> [Int] -> Int
task n xs = foldr (*) 1 list
    where list = head $ filter (\x -> sum x == 2020) (nsets n xs)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let input = map read (lines contents) :: [Int]
  print ("Task One: " ++ (show $ task 2 input))
  print ("Task Two: " ++ (show $ task 3 input))
