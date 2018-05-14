{-# LANGUAGE NoMonomorphismRestriction #-}

module SmartLaser where

import Data.Maybe
import Stream

limit = (200, 170)

smartLaserSlang :: GCodeHTTPSlang 
smartLaserSlang = GCodeHTTPSlang {
  premble = [
    "G90", 
    "\nM80",
    "\nG0F5000",
    "\nG1F1200",
    "\nS77"
  ],
  postScript = [
    "\nM81",
    "\nS0"
  ]
}

smartLazerBackend :: IO (GCodeHTTPBackend a)
smartLazerBackend = GCodeHTTPBackend <$> makePostFunc "127.0.0.1" 4444 smartLaserSlang

stateSmartLazer = postMessage <$> smartLazerBackend

streamSmartLazer = do
  s <- stateSmartLazer
  return (\m -> do
    c <- s m
    (c)
    )
  
