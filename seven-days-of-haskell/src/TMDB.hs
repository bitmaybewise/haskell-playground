{-# LANGUAGE OverloadedStrings #-}

module TMDB where

import Data.Aeson (Value (..), (.:))
import Data.Aeson.Types (FromJSON (parseJSON), prependFailure, typeMismatch)
import Data.ByteString.Char8 qualified as Char8
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpJSON, setRequestBearerAuth)

data Movie = Movie
  { id :: Int,
    title :: String,
    overview :: String,
    releaseDate :: String
  }
  deriving (Show, Generic)

instance FromJSON Movie where
  parseJSON (Object v) =
    Movie
      <$> v .: "id"
      <*> v .: "title"
      <*> v .: "overview"
      <*> v .: "release_date"
  parseJSON invalid =
    prependFailure "parsing TMDB movie failed, " (typeMismatch "Object" invalid)

newtype Response = Response
  { results :: [Movie]
  }
  deriving (Show, Generic)

instance FromJSON Response

runTMBD :: String -> IO ()
runTMBD bearearToken = do
  let req =
        setRequestBearerAuth
          (Char8.pack bearearToken)
          "GET https://api.themoviedb.org/3/discover/movie?include_adult=false&include_video=false&language=en-US&page=1&sort_by=popularity.desc"
  response <- httpJSON req
  let body = getResponseBody response :: Response
      movies = map (\mv -> (title mv, releaseDate mv)) (results body)
  mapM_ print movies
