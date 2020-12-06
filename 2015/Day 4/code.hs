import Data.ByteString.Lazy.Char8 hiding (take, length, filter)
import Data.Digest.Pure.MD5
import Flux.Core

find_answer :: Int -> String -> Int -> Int
find_answer target str i
    | target == zeroes = i
    | otherwise        = find_answer target str (i + 1)
    where 
        zeroes = length (filter ((==) '0')(take target hash))
        hash = show $ md5 $ pack new_str
        new_str = str ++ (show i)

task_one :: String -> Int
task_one str = find_answer 5 str 0

task_two :: String -> Int
task_two str = find_answer 6 str 0

main :: IO ()
main = flux_main concat_all task_one task_two
