module Filters where

-- filter
rect :: [(Double, Double)] -> Double -> Double -> Double -> Double -> [(Double, Double)]
rect tr x y x1 y1 =  filter (\ t -> (&&) ((&&) ((fst t) >= x) ((fst t) <= x1)) ((&&) ((snd t) >= y) ((snd t) <= y1))) tr

circle :: [(Double, Double)] -> Double -> Double -> Double -> [(Double, Double)]
circle tr x y rad = filter (\ t-> rad > sqrt (((^) ((fst t) - x) 2) + ((^) ((snd t) - y) 2))) tr
