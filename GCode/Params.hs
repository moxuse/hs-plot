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


randomTrail n = do
  randX <- randomIO
  randY <- randomIO
  return (randX * (fst n), randY * (snd n))

randomize ls = mapM (\l -> randomTrail l) ls
