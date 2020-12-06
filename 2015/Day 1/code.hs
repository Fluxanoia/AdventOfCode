import Flux.Core

t :: Char -> Int
t '(' = 1
t ')' = -1
t _   = 0

task_one :: String -> Int
task_one str = foldr (\a b -> (t a) + b) 0 str

task_two :: String -> Int
task_two str = snd $ foldl f (0, 0) str
    where
        f :: (Int, Int) -> Char -> (Int, Int) 
        f (floor, index) char
            | floor < 0 = (floor, index)
            | otherwise = (floor + (t char), index + 1)

main :: IO ()
main = flux_main concat_all task_one task_two
