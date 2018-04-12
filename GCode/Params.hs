module Params where

import System.Random
import Stream
import SmartLaser
-- import Diagrams.TwoD.Types

truncateTrail :: Trail -> Trail
truncateTrail t
  | fst t < 0 = (0, snd t)
  | snd t < 0 = (fst t, 0)
  | fst t > fst limit = (fst limit, snd t)
  | snd t > snd limit = (fst t, snd limit)
  | otherwise = t

pt t = truncateTrail (truncateTrail t)

ptSin :: Double -> Stroke -> Stroke
ptSin rad xs = map (\ x -> pt ((sin (fst x)) * rad, (sin (snd x)) * rad)) xs

generateX n = map (pt) [(x, 0) | x <- [0..n]]

generateY n = map (pt) [(0, y) | y <- [0..n]]

translate' :: (Double, Double) -> Stroke -> Stroke
translate' t ls = map (\ l -> pt ((fst l) + (fst t), (snd l) + (snd t))) ls

sinX :: Double -> Stroke -> Stroke
sinX rad ls = map (\ l -> pt (sin (fst l) * rad, (snd l))) ls

sinY :: Double -> Stroke -> Stroke
sinY rad ls = map (\ l -> pt ((fst l), sin (snd l) * rad)) ls

randomize :: Double -> Stroke -> Stroke
randomize fact ls = map (\ l -> pt ((randp 1 fact) + (fst l), (randp (2 + 8) fact) + (snd l) )) ls

randp s mx = head $ take 1 $ randomRs (0, mx) (mkStdGen s) :: Double
