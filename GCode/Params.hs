module Params where

import Diagrams.TwoD.Types
import System.Random
import Stream
import SmartLaser

truncateTrail :: Trail -> Trail
truncateTrail t
  | fst t < 0 = (0, snd t)
  | snd t < 0 = (fst t, 0)
  | fst t > fst limit = (fst limit, snd t)
  | snd t > snd limit = (fst t, snd limit)
  | otherwise = t

pt t = truncateTrail (truncateTrail t)

ptSin :: Double -> [Trail] -> [Trail]
ptSin rad xs = map (\ x -> pt ((sin (fst x)) * rad, (sin (snd x)) * rad)) xs

generateX n = [(x, 0) | x <- [0..n]]

generateY n = [(0, y) | y <- [0..n]]

translate' :: (Double, Double) -> [Trail] -> [Trail]
translate' t ls = map (\ l -> ((fst l) + (fst t), (snd l) + (snd t))) ls

sin' :: Double -> [Trail] -> [Trail]
sin' rad = map (\ l -> ((fst l), (snd l))) ls

randomize :: Double -> [Trail] -> [Trail]
randomize fact ls = map (\ l -> ((randp 1 fact) + (fst l), (randp (2 + 8) fact) + (snd l) )) ls

randp s mx = head $ take 1 $ randomRs (0, mx) (mkStdGen s) :: Double
