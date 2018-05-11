                           module Params where

import System.Random
import Stream
import SmartLaser
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

generate n = [(x, x - 1) | x <- [0..n]]

fill n m = [(x, 0) | x <- [0..n], y <- [0..m]]

-- filter



-- modifire

-- ptSin :: Double -> Stroke -> Stroke
ptSin rad xs = map (\ x -> (cos (fst x) * rad, (sin (snd x)) * rad)) xs

-- translate' :: (Double, Double) -> Stroke -> Stroke
translate' t ls = map (\ l -> ((fst l) + (fst t), (snd l) + (snd t))) ls

-- scale' :: (Double, Double) -> Stroke -> Stroke
scale' t ls = map (\ l -> ((fst l) * (fst t), (snd l) * (snd t))) ls

-- sinX :: Double -> Stroke -> Stroke
sinX freq ls = map (\ l -> (sin ((fst l) * freq), (snd l))) ls

-- sinY :: Double -> Stroke -> Stroke
sinY freq ls = map (\ l -> ((fst l), sin ((snd l) * freq))) ls

randomTrail n = do
  randX <- randomIO
  randY <- randomIO
  return (randX * (fst n), randY * (snd n))

randomize ls = mapM (\l -> randomTrail l) ls
