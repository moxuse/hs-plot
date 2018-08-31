                           module Params where

import System.Random
import Stream
import SmartLaser
import Data.Array
-- import Diagrams.TwoD.Types

-- limit

truncateTrail :: Trail -> Trail
truncateTrail t
  | fst t < 0 = (0, snd t)
  | snd t < 0 = (fst t, 0)
  | fst t > fst limit = (fst limit, snd t)
  | snd t > snd limit = (fst t, snd limit)
  | otherwise = t

doubleTruncateTrail t = truncateTrail (truncateTrail t)

-- generator
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

generate n = [(x, x - 1) | x <- [0..n]]

fill n m = [(x, 0) | x <- [0..n], y <- [0..m]]

stripe :: [(Double, Double)]
stripe = concat (map (\y -> concat([(x, y) | x <- (reverse' [0..(fst limit)])] : [(x, y + 1) | x <- [0..(fst limit)]] : [])) [0, 2..(snd limit)])

-- filter
rect :: [(Double, Double)] -> Double -> Double -> Double -> Double -> [(Double, Double)]
rect tr x y x1 y1 =  filter (\ t -> (&&) ((&&) ((fst t) >= x) ((fst t) <= x1)) ((&&) ((snd t) >= y) ((snd t) <= y1))) tr

circle :: [(Double, Double)] -> Double -> Double -> Double -> [(Double, Double)]
circle tr x y rad = filter (\ t-> rad > sqrt (((^) ((fst t) - x) 2) + ((^) ((snd t) - y) 2))) tr

-- modifire

-- ptSin :: Double -> Stroke -> Stroke
ptSin rad xs = map (\ x -> (cos (fst x) * rad, (sin (snd x)) * rad)) xs

-- translate' :: (Double, Double) -> Stroke -> Stroke
translate' t ls = map (\ l -> ((fst l) + (fst t), (snd l) + (snd t))) ls

-- scale' :: (Double, Double) -> Stroke -> Stroke
scale' t ls = map (\ l -> ((fst l) * (fst t), (snd l) * (snd t))) ls

-- sinX :: Double -> Stroke -> Stroke
sin' freq ls = map (\ tr -> ( sin((snd tr) * (fst freq)) + (fst (fst tr)) , sin((snd tr) * (snd freq)) + (snd (fst tr)) ) ) (zip ls (map fromIntegral [0.. (length ls)-1]))

cos' freq ls = map (\ tr -> ( cos((snd tr) * (fst freq)) + (fst (fst tr)) , cos((snd tr) * (snd freq)) + (snd (fst tr)) ) ) (zip ls (map fromIntegral [0.. (length ls)-1]))

-- cos freq ls = map (\ tr -> (cos ((fst l) * freq), (snd l))) ls

-- sinY :: Double -> Stroke -> Stroke


-- sine tr x y = map (\ t -> sin(fst (fst t) * (snd t)) ) [(x, i) | x <- tr, i = ]
--  in JS
-- sin = (stroke, [x, y]) => {
--   let i = 0;
--   return stroke.map((tr) => {
--     const v = [Math.sin(i * x) + tr[0], Math.sin(i * y) + tr[1]];
--     i++;
--     return v;
--   })
-- }



randomTrail n = do
  randX <- randomIO
  randY <- randomIO
  return (randX * (fst n), randY * (snd n))

randomize ls = mapM (\l -> randomTrail l) ls
