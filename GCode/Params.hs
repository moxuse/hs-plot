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

randp s = head $ take 1 $ randomRs ((fst limit), (snd limit)) (mkStdGen s) :: Double
