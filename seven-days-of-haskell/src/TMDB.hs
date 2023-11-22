{-# LANGUAGE OverloadedStrings #-}

module TMDB where

import Data.Aeson (Value)
import Data.ByteString.Char8 qualified as Char8
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode, httpJSON, setRequestBearerAuth)

runTMBD :: String -> IO ()
runTMBD bearearToken = do
  let req =
        setRequestBearerAuth
          (Char8.pack bearearToken)
          "GET https://api.themoviedb.org/3/discover/movie?include_adult=false&include_video=false&language=en-US&page=1&sort_by=popularity.desc"
  response <- httpJSON req
  let body = getResponseBody response :: Value
      resCode = getResponseStatusCode response
  putStrLn $ "responseCode=" ++ show resCode
  putStrLn "responseBody"
  print body
