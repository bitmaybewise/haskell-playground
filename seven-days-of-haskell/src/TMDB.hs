{-# LANGUAGE OverloadedStrings #-}

module TMDB where

import Data.Aeson (Value (..), (.:))
import Data.Aeson.Types (FromJSON (parseJSON), Parser, prependFailure, typeMismatch)
import Data.ByteString.Char8 qualified as Char8
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpJSON, setRequestBearerAuth)

data Movie = Movie
  { id :: Int,
    title :: String,
    releaseDate :: String,
    posterURL :: String,
    voteAverage :: Float
  }
  deriving (Show)

posterBaseURL :: String
posterBaseURL = "https://image.tmdb.org/t/p/w500"

instance FromJSON Movie where
  parseJSON :: Value -> Parser Movie
  parseJSON (Object v) =
    Movie
      <$> v .: "id"
      <*> v .: "title"
      <*> v .: "release_date"
      <*> ( (posterBaseURL ++)
              <$> v .: "poster_path"
          )
      <*> v .: "vote_average"
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
  mapM_ print $ results body
