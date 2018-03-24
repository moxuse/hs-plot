{-# LANGUAGE OverloadedStrings #-}

module Stream where

import qualified Data.ByteString.Lazy.Char8 as L8
import Network
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Simple (getResponseHeader, getResponseStatusCode)
import Data.Maybe
import qualified Data.Map.Strict as Map
import Diagrams.TwoD.Types
import Data.ByteString.Char8 as C8 (pack)

data Stroke = S { name :: String, from :: V2 Double, to :: V2 Double } deriving (Show)

type PostMessageFunc = [Stroke] -> IO (IO ())

data GCodeHTTPSlang = GCodeHTTPSlang {
  premble :: [String],
  suffix :: [String]
  }

data GCodeHTTPBackend a = GCodeHTTPBackend {
  postMessage :: PostMessageFunc  
}

post :: String -> String -> IO ()
post s payload = do
  print payload
  request <- parseRequest s
  manager <- newManager defaultManagerSettings
  res <- flip httpLbs manager =<< 
    (formDataBody [
        partBS "job_data" (pack payload)
      ] request)
  putStrLn $ "The status code was: " ++ show (getResponseStatusCode res)
  print $ getResponseHeader "Content-Type" res
  return ()


jobData :: GCodeHTTPSlang -> [String] -> String
jobData slang jobs = (prembleStr slang) ++ (foldl (++) "" jobs) ++ (suffixStr slang)

prembleStr sl = (foldl (++) "" (premble sl))
suffixStr sl = (foldl (++) "" (suffix sl))

toGCode :: Stroke -> String
toGCode x 
  | nameOfStroke x == "move" = vecStrOfStroke x False
  | nameOfStroke x == "line" = vecStrOfStroke x True
  | otherwise = "\nG0"

nameOfStroke x = name x
vecStrOfStroke x t 
  | t == True = "\nG0" ++ vecToStr (from x) ++ "\nG1" ++ vecToStr (to x)
  | t == False = "\nG0" ++ vecToStr (from x)

vecToStr :: V2 Double -> String
vecToStr v = "X" ++ show (fst (unr2 v)) ++ "Y" ++ show (snd (unr2 v))

toGCodeMap :: [Stroke] -> [String]
toGCodeMap m = map (toGCode) m
    
makePostFunc :: String -> Int -> GCodeHTTPSlang -> IO (PostMessageFunc)
makePostFunc address port slang = do
  let s = "POST http://" ++ address  ++ ":" ++ show(port) ++ "/gcode"
  return (\ m -> do
    let payload' = jobData slang (toGCodeMap m)
    return $ post s payload'
    )

