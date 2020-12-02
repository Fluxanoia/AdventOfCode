module Flux.Core where

nsets :: Int -> [a] -> [[a]]
nsets _ []     = []
nsets 0 _      = []
nsets 1 xs     = (map (\x -> [x]) xs)
nsets n (x:xs) = (map ((:) x) (nsets (n - 1) xs)) ++ (nsets n xs)
