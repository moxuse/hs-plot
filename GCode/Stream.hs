{-# LANGUAGE OverloadedStrings #-}

module Stream where

import Network
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Simple (getResponseHeader, getResponseStatusCode)
import Diagrams.TwoD.Types
import Data.ByteString.Char8 as C8 (pack)

type Trail = (Double, Double)

type Stroke = [Trail]

type PostMessageFunc = [Stroke] -> IO (IO ())

data GCodeHTTPSlang = GCodeHTTPSlang {
  premble :: [String],
  postScript :: [String]
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
jobData slang jobs = (prembleStr slang) ++ (foldl (++) "" jobs) ++ (postScriptStr slang)

prembleStr sl = (foldl (++) "" (premble sl))
postScriptStr sl = (foldl (++) "" (postScript sl))

toGCode :: Stroke -> String
toGCode (x:xs) = (trailStrOfStroke x) ++ (foldl (\ s d -> (trailStrOfStroke' d) ++ s) "" xs)

trailStrOfStroke x = "\nG1" ++ trailToStr x
trailStrOfStroke' x = "\nG0" ++ trailToStr x

trailToStr :: Trail -> String
trailToStr v = "X" ++ show (fst v) ++ "Y" ++ show (snd v)

toGCodeMap :: [Stroke] -> [String]
toGCodeMap m = map (toGCode) m
    
makePostFunc :: String -> Int -> GCodeHTTPSlang -> IO (PostMessageFunc)
makePostFunc address port slang = do
  let s = "POST http://" ++ address  ++ ":" ++ show(port) ++ "/gcode"
  return (\ m -> do
    let payload' = jobData slang (toGCodeMap m)
    return $ post s payload'
    )

