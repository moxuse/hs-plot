module Generators where

import SmartLaser

-- generator
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

generate n = [(x, x - 1) | x <- [0..n]]

fill n m = [(x, 0) | x <- [0..n], y <- [0..m]]

stripe :: [(Double, Double)]
stripe = concat (map (\y -> concat([(x, y) | x <- (reverse' [0..(fst limit)])] : [(x, y + 1) | x <- [0..(fst limit)]] : [])) [0, 2..(snd limit)])
