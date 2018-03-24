module Params where

import Diagrams.TwoD.Types
import System.Random
import Stream
import SmartLaser

truncateVec v
  | (fst (unr2 v)) < 0 = V2 0 (snd (unr2 v))
  | (snd (unr2 v)) < 0 = V2 (fst (unr2 v)) 0
  | (fst (unr2 v)) > fst limit = V2 (fst limit) (snd (unr2 v))
  | (snd (unr2 v)) > snd limit = V2 (fst (unr2 v)) (snd limit) 
  | otherwise = v

lineto v1 v2 = S "line" (truncateVec $ truncateVec v1) (truncateVec $ truncateVec v2)

moveto v1 = S "move" (truncateVec $ truncateVec v1) (truncateVec $ truncateVec v1)

randp s = head $ take 1 $ randomRs ((fst limit), (snd limit)) (mkStdGen s) :: Double
