{-# LANGUAGE OverloadedStrings #-}
module Posttest
where

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson (object, (.=), encode)

game :: String -> IO ()
game gameId = do
  manager <- newManager defaultManagerSettings
  
  postRequestUrl <- parseUrl $ "http://tictactoe.homedir.eu/game/" ++ gameId ++ "/player/1" 
  let request = postRequestUrl { 
  	 method = "POST"
   , requestBody = "d1:1d1:v1:x1:xi0e1:yi0eee"
   , requestHeaders = [("Content-Type", "application/bencode+map")] }

  response <- httpLbs request manager
  putStrLn "POST:"
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)

  print $ responseBody response

  getRequestUrl <- parseUrl $ "http://tictactoe.homedir.eu/game/" ++ gameId ++ "/player/2" 
  let getRequest = getRequestUrl {
  	  method = "GET"
    , requestHeaders = [("Accept", "application/bencode+map")] }

  getResponse <- httpLbs getRequest manager
  putStrLn "GET:"
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus getResponse)

  print $ responseBody getResponse


